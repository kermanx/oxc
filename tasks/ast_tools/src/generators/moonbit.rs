//! Generator for MoonBit type definitions for all AST types.

use std::borrow::Cow;

use itertools::Itertools;
use phf::phf_set;

use crate::{
    Codegen, Generator, MOONBIT_DEFINITIONS_PATH,
    derives::estree::{
        get_struct_field_name, should_flatten_field, should_skip_enum_variant, should_skip_field,
    },
    output::Output,
    schema::{Def, EnumDef, FieldDef, Schema, StructDef, TypeDef},
    utils::{FxIndexSet, format_cow, write_it},
};

use super::define_generator;

/// Generator for MoonBit type definitions.
pub struct MoonBitGenerator;

define_generator!(MoonBitGenerator);

impl Generator for MoonBitGenerator {
    /// Generate MoonBit type definitions for all AST types.
    fn generate(&self, schema: &Schema, codegen: &Codegen) -> Output {
        let estree_derive_id = codegen.get_derive_id_by_name("ESTree");

        let mut code = String::new();
        let mut ast_node_names: Vec<String> = vec![];
        for type_def in &schema.types {
            if type_def.generates_derive(estree_derive_id) {
                generate_mbt_type_def(type_def, &mut code, &mut ast_node_names, schema);
            }
        }

        Output::MoonBit { path: MOONBIT_DEFINITIONS_PATH.to_string(), code }
    }
}

/// Generate MoonBit type definition for a struct or enum.
///
/// Push type defs to `code`.
fn generate_mbt_type_def(
    type_def: &TypeDef,
    code: &mut String,
    ast_node_names: &mut Vec<String>,
    schema: &Schema,
) {
    // Skip MoonBit def generation if `#[estree(no_ts_def)]` attribute
    let no_ts_def = match type_def {
        TypeDef::Struct(struct_def) => &struct_def.estree.no_ts_def,
        TypeDef::Enum(enum_def) => &enum_def.estree.no_ts_def,
        _ => unreachable!(),
    };

    if !no_ts_def {
        let ts_def = match type_def {
            TypeDef::Struct(struct_def) => {
                generate_mbt_type_def_for_struct(struct_def, ast_node_names, schema)
            }
            TypeDef::Enum(enum_def) => generate_mbt_type_def_for_enum(enum_def, schema),
            _ => unreachable!(),
        };

        if let Some(ts_def) = ts_def {
            write_it!(code, "{ts_def};\n\n");
        }
    }

    // Add additional custom MoonBit def if provided via `#[estree(add_ts_def = "...")]` attribute
    let add_ts_def = match type_def {
        TypeDef::Struct(struct_def) => &struct_def.estree.add_ts_def,
        TypeDef::Enum(enum_def) => &enum_def.estree.add_ts_def,
        _ => unreachable!(),
    };
    if let Some(add_ts_def) = add_ts_def {
        write_it!(code, "export {add_ts_def};\n\n");
    }
}

/// Generate MoonBit type definition for a struct.
fn generate_mbt_type_def_for_struct(
    struct_def: &StructDef,
    ast_node_names: &mut Vec<String>,
    schema: &Schema,
) -> Option<String> {
    // If struct marked with `#[estree(ts_alias = "...")]`, then it needs no type def
    if struct_def.estree.ts_alias.is_some() {
        return None;
    }

    // If struct has a converter defined with `#[estree(via = Converter)]` and that converter defines
    // a type alias, then it needs no type def
    if let Some(converter_name) = &struct_def.estree.via {
        if get_mbt_type_for_converter(converter_name, schema).is_some() {
            return None;
        }
    }

    // If struct is marked as `#[estree(flatten)]`, and only has a single field which isn't skipped,
    // don't generate a type def. That single field will be inserted inline into structs which include
    // this one rather than them extending this type.
    if struct_def.estree.flatten && get_single_field(struct_def, schema).is_some() {
        return None;
    }

    let type_name = struct_def.name();
    let mut fields_str = String::new();
    let mut extends = vec![];

    if !struct_def.estree.no_type {
        ast_node_names.push(type_name.to_string());
    }

    let mut output_as_type = false;

    for &field_index in &struct_def.estree.field_indices {
        let field_index = field_index as usize;
        if let Some(field) = struct_def.fields.get(field_index) {
            generate_mbt_type_def_for_struct_field(
                struct_def,
                field,
                &mut fields_str,
                &mut extends,
                &mut output_as_type,
                schema,
            );
        }
    }

    let ts_def = if output_as_type {
        format!("pub typealias {type_name} = ({{{fields_str}\n}}) & {};", extends.join(" & "))
    } else {
        format!("pub struct {type_name} {{{fields_str}\n}}")
    };
    Some(ts_def)
}

/// Generate MoonBit type definition for a struct field.
///
/// Field definition is appended to `fields_str` or `extends`.
fn generate_mbt_type_def_for_struct_field<'s>(
    struct_def: &'s StructDef,
    field: &'s FieldDef,
    fields_str: &mut String,
    extends: &mut Vec<Cow<'s, str>>,
    output_as_type: &mut bool,
    schema: &'s Schema,
) {
    if should_skip_field(field, schema) {
        return;
    }

    generate_mbt_type_def_for_struct_field_impl(
        struct_def,
        field,
        fields_str,
        extends,
        output_as_type,
        schema,
    );
}

fn generate_mbt_type_def_for_struct_field_impl<'s>(
    struct_def: &'s StructDef,
    field: &'s FieldDef,
    fields_str: &mut String,
    extends: &mut Vec<Cow<'s, str>>,
    output_as_type: &mut bool,
    schema: &'s Schema,
) {
    if let Some(prepend_field_index) = field.estree.prepend_field_index {
        generate_mbt_type_def_for_struct_field_impl(
            struct_def,
            &struct_def.fields[prepend_field_index],
            fields_str,
            extends,
            output_as_type,
            schema,
        );
    }
    if let Some(append_field_index) = field.estree.append_field_index {
        generate_mbt_type_def_for_struct_field_impl(
            struct_def,
            &struct_def.fields[append_field_index],
            fields_str,
            extends,
            output_as_type,
            schema,
        );
    }

    let field_type_name = if let Some(converter_name) = &field.estree.via {
        let Some(ts_type) = get_mbt_type_for_converter(converter_name, schema) else {
            panic!("No `ts_type` provided for ESTree converter `{converter_name}`");
        };
        Cow::Borrowed(ts_type)
    } else {
        get_field_type_name(field, schema)
    };

    if should_flatten_field(field, schema) {
        if let TypeDef::Struct(field_type) = field.type_def(schema) {
            if let Some(flatten_field) = get_single_field(field_type, schema) {
                // Only one field to flatten. Add it as a field on the parent type, instead of extending.
                generate_mbt_type_def_for_struct_field_impl(
                    field_type,
                    flatten_field,
                    fields_str,
                    extends,
                    output_as_type,
                    schema,
                );
                return;
            }
        }

        // need `type` instead of `interface` when flattening BindingPattern
        if field_type_name.contains('|') || field_type_name == "BindingPattern" {
            *output_as_type = true;
        }
        extends.push(field_type_name);
        return;
    }

    let field_camel_name = get_struct_field_name(field);
    if RESERVED_WORDS.contains(&field_camel_name) {
        write_it!(fields_str, "\n\t_{field_camel_name}: {field_type_name};");
    } else {
        write_it!(fields_str, "\n\t{field_camel_name}: {field_type_name};");
    }
}

/// Generate MoonBit type definition for an extra struct field
/// specified with `#[estree(add_fields(...))]`.
fn generate_mbt_type_def_for_added_struct_field(
    field_name: &str,
    converter_name: &str,
    fields_str: &mut String,
    schema: &Schema,
) {
    let converter = schema.meta_by_name(converter_name);
    let Some(mbt_type) = converter.estree.ts_type.as_deref() else {
        panic!("No `ts_type` provided for ESTree converter `{converter_name}`");
    };
    let mbt_type = if converter.estree.is_js || converter.estree.is_ts {
        format!("Option[{mbt_type}]")
    } else {
        mbt_type.to_string()
    };
    write_it!(fields_str, "\n\t{field_name}: {mbt_type};");
}

/// Get the TS type definition for a converter.
///
/// Converters are specified with `#[estree(add_fields(field_name = converter_name))]`
/// and `#[estree(via = converter_name)]`.
fn get_mbt_type_for_converter<'s>(converter_name: &str, schema: &'s Schema) -> Option<&'s str> {
    let converter = schema.meta_by_name(converter_name);
    converter.estree.ts_type.as_deref()
}

/// Generate MoonBit type definition for an enum.
fn generate_mbt_type_def_for_enum(enum_def: &EnumDef, schema: &Schema) -> Option<String> {
    // If enum marked with `#[estree(ts_alias = "...")]`, then it needs no type def
    if enum_def.estree.ts_alias.is_some() {
        return None;
    }

    // FIXME:
    // // If enum has a converter defined with `#[estree(via = Converter)]` and that converter defines
    // // a type alias, then it needs no type def
    // if let Some(converter_name) = &enum_def.estree.via {
    //     if get_ts_type_for_converter(converter_name, schema).is_some() {
    //         return None;
    //     }
    // }

    // Get variant type names.
    // Collect into `FxIndexSet` to filter out duplicates.
    let mut variant_type_names = enum_def
        .variants
        .iter()
        .filter(|variant| !should_skip_enum_variant(variant))
        .map(|variant| {
            if let Some(converter_name) = &variant.estree.via {
                Cow::Borrowed(get_mbt_type_for_converter(converter_name, schema).unwrap())
            } else if let Some(variant_type) = variant.field_type(schema) {
                let name = mbt_type_name(variant_type, schema);
                format!("\t{name}({name})").into()
            } else {
                format!("\t{}", variant.name()).into()
            }
        })
        .collect::<FxIndexSet<_>>();

    variant_type_names.extend(
        enum_def.inherits_types(schema).map(|inherited_type| mbt_type_name(inherited_type, schema)),
    );

    let union = variant_type_names.iter().join("\n");

    let enum_name = enum_def.name();
    Some(format!("pub enum {enum_name} {{\n{union}\n}}"))
}

/// Get TS type name for a type.
fn mbt_type_name<'s>(type_def: &'s TypeDef, schema: &'s Schema) -> Cow<'s, str> {
    match type_def {
        TypeDef::Struct(struct_def) => {
            if let Some(ts_alias) = &struct_def.estree.ts_alias {
                Cow::Borrowed(ts_alias)
            } else {
                if let Some(converter_name) = &struct_def.estree.via {
                    if let Some(type_name) = get_mbt_type_for_converter(converter_name, schema) {
                        return Cow::Borrowed(type_name);
                    }
                }
                Cow::Borrowed(struct_def.name())
            }
        }
        TypeDef::Enum(enum_def) => {
            if let Some(ts_alias) = &enum_def.estree.ts_alias {
                Cow::Borrowed(ts_alias)
            } else {
                if let Some(converter_name) = &enum_def.estree.via {
                    if let Some(type_name) = get_mbt_type_for_converter(converter_name, schema) {
                        return Cow::Borrowed(type_name);
                    }
                }
                Cow::Borrowed(enum_def.name())
            }
        }
        TypeDef::Primitive(primitive_def) => Cow::Borrowed(match primitive_def.name() {
            "i16" => "Int16",
            "i32" => "Int",
            "i64" => "Int64",
            "u16" => "UInt16",
            "u32" => "UInt",
            "u64" => "UInt64",
            "f32" => "Float",
            "f64" => "Double",
            "bool" => "Bool",
            "string" | "Atom" => "String",
            name => name,
        }),
        TypeDef::Option(option_def) => {
            format_cow!("Option[{}]", mbt_type_name(option_def.inner_type(schema), schema))
        }
        TypeDef::Vec(vec_def) => {
            format_cow!("Array[{}]", mbt_type_name(vec_def.inner_type(schema), schema))
        }
        TypeDef::Box(box_def) => mbt_type_name(box_def.inner_type(schema), schema),
        TypeDef::Cell(cell_def) => mbt_type_name(cell_def.inner_type(schema), schema),
    }
}

/// Get type name for a field.
fn get_field_type_name<'s>(field: &'s FieldDef, schema: &'s Schema) -> Cow<'s, str> {
    if let Some(ts_type) = field.estree.ts_type.as_deref() {
        Cow::Borrowed(ts_type)
    } else {
        let field_type = field.type_def(schema);
        mbt_type_name(field_type, schema)
    }
}

/// If struct has only a single unskipped field, return it.
///
/// If no fields, or more than 1 unskipped field, returns `None`.
fn get_single_field<'s>(struct_def: &'s StructDef, schema: &Schema) -> Option<&'s FieldDef> {
    let mut fields_which_are_not_skipped =
        struct_def.fields.iter().filter(|field| !should_skip_field(field, schema));

    if let Some(field) = fields_which_are_not_skipped.next() {
        if fields_which_are_not_skipped.next().is_none() {
            return Some(field);
        }
    }
    None
}

static RESERVED_WORDS: phf::Set<&'static str> = phf_set!["readonly", "type", "async", "null", "test"];
