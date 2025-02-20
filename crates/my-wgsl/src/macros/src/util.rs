use quote::ToTokens;
use syn::{spanned::Spanned, Error, Expr, Field, Ident, ItemStruct, Lit, Meta, Result, Type};

pub(crate) const fn round_up_by_align(value: usize, align: usize) -> usize {
    (value + align - 1) & (!(align - 1))
}

pub(crate) fn expr_to_integer(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Int(lit_int) => lit_int.base10_parse::<i64>().ok(),
            _ => None,
        },
        _ => None,
    }
}

pub(crate) fn elem_type(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Array(ty) => Some(&ty.elem),
        Type::Slice(ty) => Some(&ty.elem),
        _ => None,
    }
}

pub(crate) fn last_type_path_ident(ty: &Type) -> Result<&Ident> {
    match ty {
        Type::Path(t) => t
            .path
            .segments
            .last()
            .map(|seg| &seg.ident)
            .ok_or(Error::new(ty.span(), "invalid type path")),
        _ => Err(Error::new(ty.span(), "not a path type")),
    }
}

pub(crate) fn wgsl_type_str(ty: &Type) -> String {
    match ty {
        Type::Path(ty) => {
            if let Some(last) = ty.path.segments.last() {
                _wgsl_type_str(last.ident.to_string())
            } else {
                ty.to_token_stream().to_string()
            }
        }
        // [T; N] => array<T,N>
        Type::Array(ty) => {
            let mut buf = String::with_capacity("array<T,N>".len());
            buf.push_str("array<");
            buf.push_str(&wgsl_type_str(&ty.elem));
            buf.push(',');
            buf.push_str(&ty.len.to_token_stream().to_string());
            buf.push('>');
            buf
        }
        // [T] => array<T>
        Type::Slice(ty) => {
            let mut buf = String::with_capacity("array<T>".len());
            buf.push_str("array<");
            buf.push_str(&wgsl_type_str(&ty.elem));
            buf.push('>');
            buf
        }
        o => o.to_token_stream().to_string(),
    }
}

#[rustfmt::skip]
fn _wgsl_type_str(s: String) -> String {
    match s.as_str() {
        "Bool" => "bool",
        "Vec2i" => "vec2i", "Vec3i" => "vec3i", "Vec4i" => "vec4i",
        "Vec2u" => "vec2u", "Vec3u" => "vec3u", "Vec4u" => "vec4u",
        "Vec2f" => "vec2f", "Vec3f" => "vec3f", "Vec4f" => "vec4f",
        "WideVec2i" => "vec2i", "WideVec3i" => "vec3i", "WideVec4i" => "vec4i",
        "WideVec2u" => "vec2u", "WideVec3u" => "vec3u", "WideVec4u" => "vec4u",
        "WideVec2f" => "vec2f", "WideVec3f" => "vec3f", "WideVec4f" => "vec4f",
        "Mat2x2f" => "mat2x2f", "Mat2x3f" => "mat2x3f", "Mat2x4f" => "mat2x4f",
        "Mat3x2f" => "mat3x2f", "Mat3x3f" => "mat3x3f", "Mat3x4f" => "mat3x4f",
        "Mat4x2f" => "mat4x2f", "Mat4x3f" => "mat4x3f", "Mat4x4f" => "mat4x4f",
        _ => return s,
    }.to_owned()
}

pub(crate) trait GetAttributeValue {
    fn get_attribute_value(&self, outer: &str) -> Option<String>;
}

impl GetAttributeValue for Field {
    fn get_attribute_value(&self, outer: &str) -> Option<String> {
        let a = self.attrs.iter().find(|a| a.path().is_ident(outer))?;
        if let Meta::List(l) = &a.meta {
            Some(l.tokens.to_string())
        } else {
            None
        }
    }
}

pub(crate) trait ContainsAttribute {
    fn contains_attribute(&self, attr_ident: &str) -> bool;
}

impl ContainsAttribute for ItemStruct {
    fn contains_attribute(&self, attr_ident: &str) -> bool {
        self.attrs
            .iter()
            .any(|attr| attr.path().is_ident(attr_ident))
    }
}

pub(crate) trait ToUppercase {
    fn to_uppercase(&self) -> Self;
}

impl ToUppercase for Ident {
    fn to_uppercase(&self) -> Self {
        Ident::new(&self.to_string().to_uppercase(), self.span())
    }
}
