//! A library for infering a data schema from example values.

#![forbid(missing_docs, unsafe_code)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(feature = "serde-json")]
use serde_json::Value;
use smallvec::{SmallVec, smallvec};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::Debug;
use std::ops::Add;

/// Schema representing a top-level value. This combines a set of possible
/// [`Type`] variants with a nullability flag. This is also the inner type of an
/// array and of an object value.
#[derive(Clone, PartialEq, Eq)]
pub struct Schema {
    nullable: bool,
    types: SmallVec<[Type; 1]>,
}

impl Debug for Schema {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.nullable {
            fmt.write_str("?")?;
        }
        match self.types.len() {
            0 => fmt.write_str("unknown"),
            1 => self.types[0].fmt(fmt),
            _ => {
                if self.nullable {
                    fmt.write_str("(")?;
                }
                for (i, t) in self.types.iter().enumerate() {
                    if i != 0 {
                        fmt.write_str(" | ")?;
                    }
                    t.fmt(fmt)?;
                }
                if self.nullable {
                    fmt.write_str(")")?;
                }
                Ok(())
            }
        }
    }
}

impl Schema {
    /// Infers the schema corresponding to the given JSON value.
    #[cfg(feature = "serde-json")]
    pub fn infer(value: &Value) -> Self {
        match value {
            Value::Null => Schema::null(),
            Value::Bool(x) => Schema::new(Type::bool(*x)),
            Value::Number(x) => Schema::new(Type::Number {
                fits_u64: x.is_u64(),
                fits_i64: x.is_i64(),
            }),
            Value::String(s) => Schema::new(Type::string(s)),
            Value::Array(a) => Schema::new(Type::Array(Box::new(Schema::unify_all(
                a.iter().map(Schema::infer),
            )))),
            Value::Object(o) => Schema::new(Type::Object(
                o.iter()
                    .map(|(k, v)| (k.clone(), Schema::infer(v)))
                    .collect(),
            )),
        }
    }

    /// Converts objects with at least `threshold` keys into maps.
    pub fn merge_maps(&mut self, threshold: usize) -> bool {
        self.types
            .iter_mut()
            .map(|typ| typ.merge_maps(threshold))
            .fold(false, |x, y| x | y)
    }

    /// Creates a schema for the given non-null type.
    pub fn new(typ: Type) -> Self {
        Schema {
            nullable: false,
            types: smallvec![typ],
        }
    }

    /// Creates a schema for the given nullable type.
    pub fn nullable(typ: Type) -> Self {
        Schema {
            nullable: true,
            types: smallvec![typ],
        }
    }

    /// Creates a schema for an unknown type.
    pub fn unknown() -> Self {
        Schema {
            nullable: false,
            types: smallvec![],
        }
    }

    /// Creates a schema for the `null` value, i.e. an unknown nullable type.
    pub fn null() -> Self {
        Schema {
            nullable: true,
            types: smallvec![],
        }
    }

    /// Creates a JSON schema with the given non-null types.
    #[cfg(test)]
    #[track_caller]
    fn variants(types: SmallVec<[Type; 1]>) -> Self {
        for [x, y] in types.array_windows() {
            assert!(
                x.discriminant() < y.discriminant(),
                "Failed precondition (variant types must be ordered): x < y for x = {x:?}, y = {y:?}"
            );
        }
        Schema {
            nullable: false,
            types,
        }
    }

    /// Creates a JSON schema with the given nullable types.
    #[cfg(test)]
    #[track_caller]
    fn variants_nullable(types: SmallVec<[Type; 1]>) -> Self {
        for [x, y] in types.array_windows() {
            assert!(
                x.discriminant() < y.discriminant(),
                "Failed precondition (variant types must be ordered): x < y for x = {x:?}, y = {y:?}"
            );
        }
        Schema {
            nullable: true,
            types,
        }
    }

    /// Unifies two schemas.
    pub fn unify(x: Self, y: Self) -> Self {
        let nullable = x.nullable || y.nullable;
        let types = match (x.types.is_empty(), y.types.is_empty()) {
            (false, false) => Type::unify_types(x.types, y.types),
            (false, true) => x.types,
            (true, false) => y.types,
            (true, true) => smallvec![],
        };
        Schema { nullable, types }
    }

    /// Unifies the given schemas.
    pub fn unify_all(schemas: impl Iterator<Item = Self>) -> Self {
        schemas.reduce(Schema::unify).unwrap_or(Schema::unknown())
    }
}

/// A primitive type.
#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    /// Boolean.
    Bool {
        /// Number of observed [`true`] values.
        count_true: u64,
        /// Number of observed [`false`] values.
        count_false: u64,
    },
    /// Number.
    Number {
        /// Whether this number fits in a [`u64`].
        fits_u64: bool,
        /// Whether this number fits in a [`i64`].
        fits_i64: bool,
    },
    /// UTF-8 string.
    String {
        /// Number of observed values.
        count: u64,
        /// Statistics about the string length.
        len: Stats,
    },
    /// Byte array.
    Bytes,
    /// Array.
    Array(Box<Schema>),
    /// Map.
    Map(Box<Schema>),
    /// Object.
    Object(HashMap<String, Schema>),
}

/// Statistics about observed values.
#[derive(Clone, PartialEq, Eq)]
pub struct Stats {
    /// Minimum value.
    pub min: u64,
    /// Maximum value.
    pub max: u64,
    /// Sum of values.
    pub sum: u128,
}

impl Stats {
    fn new(t: u64) -> Self {
        Self {
            min: t,
            max: t,
            sum: t.into(),
        }
    }
}

impl Add for Stats {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
            sum: self.sum + other.sum,
        }
    }
}

impl Debug for Type {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool {
                count_true,
                count_false,
            } => fmt.write_fmt(format_args!(
                "bool ({}% true)",
                *count_true as u128 * 100 / (count_true + count_false) as u128,
            )),
            Type::Number {
                fits_u64: true,
                fits_i64: _,
            } => fmt.write_str("uint"),
            Type::Number {
                fits_u64: false,
                fits_i64: true,
            } => fmt.write_str("int"),
            Type::Number {
                fits_u64: false,
                fits_i64: false,
            } => fmt.write_str("number"),
            Type::String { count, len } => fmt.write_fmt(format_args!(
                "string (len = [min: {}, avg: {}, max: {}])",
                len.min,
                len.sum / *count as u128,
                len.max
            )),
            Type::Bytes => fmt.write_str("bytes"),
            Type::Array(a) => {
                fmt.write_str("[")?;
                a.fmt(fmt)?;
                fmt.write_str("]")
            }
            Type::Map(m) => {
                fmt.write_str("{string -> ")?;
                m.fmt(fmt)?;
                fmt.write_str("}")
            }
            Type::Object(o) => {
                let mut entries: Vec<_> = o.iter().collect();
                entries.sort_by_key(|&(k, _)| k);
                fmt.debug_map().entries(entries).finish()
            }
        }
    }
}

impl Type {
    // This is needed because core::mem::Discriminant doesn't implement Ord.
    fn discriminant(&self) -> usize {
        match self {
            Type::Bool { .. } => 0,
            Type::Number { .. } => 1,
            Type::String { .. } => 2,
            Type::Bytes => 3,
            Type::Array(_) => 4,
            Type::Map(_) => 5,
            Type::Object(_) => 6,
        }
    }

    /// Converts objects with at least `threshold` keys into maps.
    pub fn merge_maps(&mut self, threshold: usize) -> bool {
        match self {
            Type::Bool { .. } | Type::Number { .. } | Type::String { .. } | Type::Bytes => false,
            Type::Array(schema) | Type::Map(schema) => schema.merge_maps(threshold),
            Type::Object(map) => {
                if map.len() < threshold {
                    map.values_mut()
                        .map(|v| v.merge_maps(threshold))
                        .fold(false, |a, b| a | b)
                } else {
                    let mut extracted = HashMap::new();
                    std::mem::swap(map, &mut extracted);

                    let mut schema = Schema::unify_all(extracted.into_values());
                    schema.merge_maps(threshold);
                    *self = Type::Map(Box::new(schema));
                    true
                }
            }
        }
    }

    /// Creates a type that contains the given value.
    pub fn bool(x: bool) -> Self {
        if x {
            Type::Bool {
                count_true: 1,
                count_false: 0,
            }
        } else {
            Type::Bool {
                count_true: 0,
                count_false: 1,
            }
        }
    }

    /// Creates a type that contains the given value.
    pub fn u64(x: u64) -> Self {
        Type::Number {
            fits_u64: true,
            fits_i64: x <= i64::MAX as u64,
        }
    }

    /// Creates a type that contains the given value.
    pub fn i64(x: i64) -> Self {
        Type::Number {
            fits_u64: x >= 0,
            fits_i64: true,
        }
    }

    /// Creates a type that contains the given value.
    pub fn f64(x: f64) -> Self {
        let integer = x.fract() == 0.0;
        Type::Number {
            fits_u64: integer && (0.0..18446744073709551616.0).contains(&x),
            fits_i64: integer && (9223372036854775808.0..9223372036854775808.0).contains(&x),
        }
    }

    /// Creates a type that contains the given value.
    pub fn string(s: &str) -> Self {
        Type::String {
            count: 1,
            len: Stats::new(s.len() as u64),
        }
    }

    /// Creates a type that contains the given value.
    pub fn bytes(_: &[u8]) -> Self {
        Type::Bytes
    }

    /// Creates a number type that can fit any integer type.
    #[cfg(test)]
    fn integer() -> Self {
        Type::Number {
            fits_u64: true,
            fits_i64: true,
        }
    }

    /// Creates an array type with the given non-null inner type.
    #[cfg(test)]
    fn array(inner: Type) -> Self {
        Type::Array(Box::new(Schema::new(inner)))
    }

    /// Creates an array type with the given nullable inner type.
    #[cfg(test)]
    fn array_nullable(inner: Type) -> Self {
        Type::Array(Box::new(Schema::nullable(inner)))
    }

    /// Creates an object type with the given non-optional fields.
    #[cfg(test)]
    fn object(fields: impl IntoIterator<Item = (impl Into<String>, Schema)>) -> Self {
        Type::Object(
            fields
                .into_iter()
                .map(|(k, schema)| (k.into(), schema))
                .collect(),
        )
    }

    /// Attempts to unify two primitive types. If the types are incompatible, an
    /// error containing the inputs is returned.
    #[expect(clippy::result_large_err)]
    fn unify(x: Self, y: Self) -> Result<Self, (Self, Self)> {
        match (x, y) {
            (
                Type::Bool {
                    count_true: a_count_true,
                    count_false: a_count_false,
                },
                Type::Bool {
                    count_true: b_count_true,
                    count_false: b_count_false,
                },
            ) => Ok(Type::Bool {
                count_true: a_count_true + b_count_true,
                count_false: a_count_false + b_count_false,
            }),
            (
                Type::Number {
                    fits_u64: a_u64,
                    fits_i64: a_i64,
                },
                Type::Number {
                    fits_u64: b_u64,
                    fits_i64: b_i64,
                },
            ) => Ok(Type::Number {
                fits_u64: a_u64 && b_u64,
                fits_i64: a_i64 && b_i64,
            }),
            (
                Type::String {
                    count: a_count,
                    len: a_len,
                },
                Type::String {
                    count: b_count,
                    len: b_len,
                },
            ) => Ok(Type::String {
                count: a_count + b_count,
                len: a_len + b_len,
            }),
            (Type::Bytes, Type::Bytes) => Ok(Type::Bytes),
            (Type::Array(a), Type::Array(b)) => Ok(Type::Array(Box::new(Schema::unify(*a, *b)))),
            (Type::Map(a), Type::Map(b)) => Ok(Type::Map(Box::new(Schema::unify(*a, *b)))),
            (Type::Object(o), Type::Object(p)) => {
                let mut tmp: HashMap<String, (Schema, bool)> =
                    o.into_iter().map(|(k, v)| (k, (v, false))).collect();
                for (k, v) in p.into_iter() {
                    match tmp.entry(k) {
                        Entry::Occupied(mut entry) => {
                            let entry_mut = entry.get_mut();
                            entry_mut.0 = Schema::unify(entry_mut.0.clone(), v);
                            entry_mut.1 = true;
                        }
                        Entry::Vacant(entry) => {
                            entry.insert((v, false));
                        }
                    }
                }
                let schema = tmp
                    .into_iter()
                    .map(|(k, (v, both))| {
                        (
                            k,
                            Schema {
                                nullable: v.nullable || !both,
                                types: v.types,
                            },
                        )
                    })
                    .collect();
                Ok(Type::Object(schema))
            }
            (x, y) => Err((x, y)),
        }
    }

    /// Unifies the given type variants.
    fn unify_types(x: SmallVec<[Self; 1]>, y: SmallVec<[Self; 1]>) -> SmallVec<[Self; 1]> {
        let mut result = SmallVec::with_capacity(x.len().max(y.len()));
        let mut itx = x.into_iter().peekable();
        let mut ity = y.into_iter().peekable();
        loop {
            let next_type = match (itx.peek(), ity.peek()) {
                (Some(a), Some(b)) => match a.discriminant().cmp(&b.discriminant()) {
                    Ordering::Less => itx.next().unwrap(),
                    Ordering::Greater => ity.next().unwrap(),
                    Ordering::Equal => Type::unify(itx.next().unwrap(), ity.next().unwrap())
                        .expect("Failed to unify types of the same category"),
                },
                (Some(_), None) => itx.next().unwrap(),
                (None, Some(_)) => ity.next().unwrap(),
                (None, None) => break,
            };
            result.push(next_type);
        }
        result
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_debug_base_types() {
        assert_eq!(
            format!(
                "{:?}",
                Type::Bool {
                    count_true: 2,
                    count_false: 3
                }
            ),
            "bool (40% true)"
        );
        assert_eq!(format!("{:?}", Type::integer()), "uint");
        assert_eq!(
            format!(
                "{:?}",
                Type::String {
                    count: 5,
                    len: Stats {
                        min: 10,
                        max: 20,
                        sum: 75
                    },
                }
            ),
            "string (len = [min: 10, avg: 15, max: 20])"
        );
        assert_eq!(format!("{:?}", Type::Bytes), "bytes");
    }

    #[test]
    fn test_debug_array() {
        assert_eq!(
            format!(
                "{:?}",
                Type::array(Type::Bool {
                    count_true: 2,
                    count_false: 5
                })
            ),
            "[bool (28% true)]"
        );
        assert_eq!(
            format!(
                "{:#?}",
                Type::array(Type::Bool {
                    count_true: 4,
                    count_false: 2
                })
            ),
            "[bool (66% true)]"
        );
        assert_eq!(
            format!("{:?}", Type::array_nullable(Type::integer())),
            "[?uint]"
        );
        assert_eq!(
            format!("{:#?}", Type::array_nullable(Type::integer())),
            "[?uint]"
        );
    }

    #[test]
    fn test_debug_object() {
        let schema = Schema::new(Type::object([
            ("apple", Schema::new(Type::integer())),
            (
                "banana",
                Schema::new(Type::Bool {
                    count_true: 4,
                    count_false: 2,
                }),
            ),
            ("cherry", Schema::null()),
            (
                "date",
                Schema::new(Type::String {
                    count: 5,
                    len: Stats {
                        min: 10,
                        max: 20,
                        sum: 75,
                    },
                }),
            ),
        ]));
        assert_eq!(
            format!("{:?}", schema),
            r#"{"apple": uint, "banana": bool (66% true), "cherry": ?unknown, "date": string (len = [min: 10, avg: 15, max: 20])}"#
        );
        assert_eq!(
            format!("{:#?}", schema),
            r#"{
    "apple": uint,
    "banana": bool (66% true),
    "cherry": ?unknown,
    "date": string (len = [min: 10, avg: 15, max: 20]),
}"#
        );
    }

    #[test]
    fn test_debug_variants() {
        let schema = Schema::variants(smallvec![
            Type::Bool {
                count_true: 5,
                count_false: 1
            },
            Type::integer(),
            Type::array(Type::Bool {
                count_true: 1,
                count_false: 1
            })
        ]);
        assert_eq!(
            format!("{:?}", schema),
            "bool (83% true) | uint | [bool (50% true)]"
        );
        assert_eq!(
            format!("{:#?}", schema),
            "bool (83% true) | uint | [bool (50% true)]"
        );

        let schema = Schema::variants_nullable(smallvec![
            Type::Bool {
                count_true: 5,
                count_false: 1
            },
            Type::integer(),
            Type::array(Type::Bool {
                count_true: 1,
                count_false: 1
            })
        ]);
        assert_eq!(
            format!("{:?}", schema),
            "?(bool (83% true) | uint | [bool (50% true)])"
        );
        assert_eq!(
            format!("{:#?}", schema),
            "?(bool (83% true) | uint | [bool (50% true)])"
        );
    }

    #[test]
    fn test_debug_mix() {
        let schema = Schema::new(Type::object([
            (
                "apple",
                Schema::variants_nullable(smallvec![
                    Type::Bool {
                        count_true: 10,
                        count_false: 0
                    },
                    Type::integer()
                ]),
            ),
            (
                "banana",
                Schema::new(Type::array(Type::Bool {
                    count_true: 5,
                    count_false: 6,
                })),
            ),
            (
                "cherry",
                Schema::new(Type::array_nullable(Type::String {
                    count: 5,
                    len: Stats {
                        min: 10,
                        max: 20,
                        sum: 75,
                    },
                })),
            ),
            (
                "date",
                Schema::new(Type::object([(
                    "eggplant",
                    Schema::nullable(Type::array(Type::integer())),
                )])),
            ),
            (
                "fig",
                Schema::new(Type::array_nullable(Type::object([(
                    "hazelnut",
                    Schema::new(Type::Bool {
                        count_true: 1,
                        count_false: 13,
                    }),
                )]))),
            ),
        ]));
        assert_eq!(
            format!("{:?}", schema),
            r#"{"apple": ?(bool (100% true) | uint), "banana": [bool (45% true)], "cherry": [?string (len = [min: 10, avg: 15, max: 20])], "date": {"eggplant": ?[uint]}, "fig": [?{"hazelnut": bool (7% true)}]}"#
        );
        assert_eq!(
            format!("{:#?}", schema),
            r#"{
    "apple": ?(bool (100% true) | uint),
    "banana": [bool (45% true)],
    "cherry": [?string (len = [min: 10, avg: 15, max: 20])],
    "date": {
        "eggplant": ?[uint],
    },
    "fig": [?{
        "hazelnut": bool (7% true),
    }],
}"#
        );
    }
}
