use std::{fmt::Display, str::FromStr};
use syn::{spanned::Spanned, Error, Expr, Ident, Lit, Path, Result, Type};

pub(crate) const fn round_up_by_align(value: usize, align: usize) -> usize {
    (value + align - 1) & (!(align - 1))
}

pub(crate) fn expr_to_number<N>(expr: &Expr) -> Result<N>
where
    N: FromStr,
    N::Err: Display,
{
    match expr {
        Expr::Lit(expr_lit) => lit_to_number(&expr_lit.lit),
        _ => Err(Error::new(expr.span(), "expected literal expression")),
    }
}

pub(crate) fn lit_to_number<N>(lit: &Lit) -> Result<N>
where
    N: FromStr,
    N::Err: Display,
{
    match lit {
        Lit::Int(lit_int) => lit_int.base10_parse::<N>(),
        Lit::Float(lit_float) => lit_float.base10_parse::<N>(),
        _ => Err(Error::new(
            lit.span(),
            "expected literal integer or floating number",
        )),
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
        Type::Path(t) => last_path_ident(&t.path),
        _ => Err(Error::new(ty.span(), "expected type path")),
    }
}

pub(crate) fn last_path_ident(path: &Path) -> Result<&Ident> {
    path.segments
        .last()
        .map(|seg| &seg.ident)
        .ok_or(Error::new(path.span(), "invalid type path"))
}

#[derive(Debug)]
pub(crate) struct TrieNode<T> {
    leaf: Option<T>,
    children: Vec<(char, TrieNode<T>)>,
}

impl<T> TrieNode<T> {
    pub(crate) fn new() -> Self {
        Self {
            leaf: None,
            children: Vec::new(),
        }
    }

    pub(crate) fn insert(&mut self, key: &str, value: T) {
        self._insert(key).leaf = Some(value);
    }

    pub(crate) fn push(&mut self, value: T)
    where
        T: AsRef<str>,
    {
        let key = value.as_ref();
        let node = self._insert(key);
        node.leaf = Some(value);
    }

    pub(crate) fn get(&self, key: &str) -> Option<&T> {
        let mut cur = self;

        for c in key.chars() {
            if let Some(next) = cur.children.iter().find(|(k, _)| *k == c) {
                cur = &next.1;
            } else {
                return None;
            }
        }

        cur.leaf.as_ref()
    }

    fn _insert(&mut self, key: &str) -> &mut TrieNode<T> {
        let mut cur = self;

        for c in key.chars() {
            if let Some(i) = cur.children.iter().position(|(k, _)| *k == c) {
                cur = &mut cur.children[i].1;
            } else {
                cur.children.push((c, TrieNode::new()));
                cur = unsafe { &mut cur.children.last_mut().unwrap_unchecked().1 };
            }
        }

        cur
    }
}
