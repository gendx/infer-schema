use anyhow::{Context, Result};
use infer_schema::Schema;
use rstest::rstest;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::PathBuf;

fn check_expected_file(serialized: &str, expected_path: &str) -> Result<()> {
    let mut f = File::open(expected_path)
        .with_context(|| format!("Failed to read file {expected_path}"))?;
    let mut expected = String::new();
    f.read_to_string(&mut expected)
        .with_context(|| format!("Failed to read file {expected_path}"))?;
    assert_eq!(serialized, expected);

    Ok(())
}

#[rstest]
fn test_infer(#[files("data/**/*.json")] path: PathBuf) -> Result<()> {
    let name = path
        .file_stem()
        .with_context(|| format!("Failed to remove directory prefix from {path:?}"))?
        .to_str()
        .with_context(|| format!("Failed to convert path to string for {path:?}"))?;

    let file =
        BufReader::new(File::open(&path).with_context(|| format!("Failed to read file {path:?}"))?);
    let value: serde_json::Value = serde_json::from_reader(file)
        .with_context(|| format!("Failed to deserialize file {path:?}"))?;

    let mut schema = Schema::infer(&value);
    schema.merge_maps(100);
    let debugged = format!("{schema:#?}");

    check_expected_file(&debugged, &format!("schemas/{name}.txt"))?;

    Ok(())
}
