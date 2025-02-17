use syn::{Expr, Ident, ItemStruct, Lit, Type};

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
