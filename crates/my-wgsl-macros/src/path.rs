use super::{traits::*, util::*};
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use std::{borrow::Cow, cell::RefCell, fmt};
use syn::{
    Error, Ident, Path, PathSegment, Result, Token, punctuated::Punctuated, spanned::Spanned,
};
use wgsl_builtin::{helper::*, prelude::*};

// === WgslPath ===

#[derive(Debug)]
pub(crate) struct WgslPath {
    pub(crate) segments: Punctuated<PathSegment, Token![::]>,
}

impl AsIdent for WgslPath {
    fn as_ident(&self) -> Result<&Ident> {
        self.segments
            .last()
            .map(|seg| &seg.ident)
            .ok_or(Error::new(self.span(), "expected non-empty path"))
    }
}

impl fmt::Display for WgslPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", path_segments_to_string(&self.segments))
    }
}

impl FromSyn<Path> for WgslPath {
    fn from_syn(input: Path) -> Result<Self> {
        Ok(Self {
            segments: input.segments,
        })
    }
}

impl ToTokens for WgslPath {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        for pair in self.segments.pairs() {
            pair.value().to_tokens(tokens);
            pair.punct().to_tokens(tokens);
        }
    }
}

impl ToWgslString for WgslPath {
    fn write_wgsl_string(&self, buf: &mut String) {
        write_path_to_buffer(self, buf);
    }
}

pub(crate) fn path_segments_to_string(segments: &Punctuated<PathSegment, Token![::]>) -> String {
    let mut path_str = String::new();
    for pair in segments.pairs() {
        path_str.push_str(&pair.value().ident.to_string());
        if pair.punct().is_some() {
            path_str.push_str("::");
        }
    }
    path_str
}

pub(crate) fn write_path_to_buffer<T: ToString>(path: &T, buf: &mut String) {
    let path_str = path.to_string();
    let mut slice = path_str.as_str();
    while !slice.is_empty() {
        if let Some(found) = to_wgsl_path(slice) {
            buf.push_str(found);
            return;
        }
        if let Some(i) = slice.find("::") {
            slice = &slice[i + 2..];
        } else {
            break;
        }
    }

    // Path not found.
    // TODO: Better approach?
    let mut window = [None, None];
    for segment in path_str.split("::") {
        window[0] = window[1];
        window[1] = Some(segment);
    }
    match window {
        [Some(before), _] => buf.push_str(before), // ... :: A :: new -> A
        [_, Some(last)] => buf.push_str(last),     // A -> A
        _ => {}
    }
}

// === PATH_NAMES ===

macro_rules! entries {
    ($root:ident, $($ty:ty),*) => {
        $(
            $root.insert(
                <$ty>::ident(),
                Cow::Borrowed(<$ty>::wgsl_ident())
            );
        )*

        $(
            for (rust, wgsl) in <$ty>::constructors() {
                $root.insert(rust, Cow::Borrowed(wgsl));
            }
        )*
    };
}

#[rustfmt::skip]
thread_local! {
    static PATH_NAMES: RefCell<TrieNode<Cow<'static, str>>> = RefCell::new({
        let mut root = TrieNode::new();

        entries!(
            root, 
            Bool, 
            Vec2i, Vec3i, Vec4i, Vec2u, Vec3u, Vec4u, Vec2f, Vec3f, Vec4f,
            WideVec2i, WideVec3i, WideVec4i,
            WideVec2u, WideVec3u, WideVec4u,
            WideVec2f, WideVec3f, WideVec4f,
            Mat2x2f, Mat2x3f, Mat2x4f,
            Mat3x2f, Mat3x3f, Mat3x4f,
            Mat4x2f, Mat4x3f, Mat4x4f
        );

        root.insert("i32", "i32".into());
        root.insert("u32", "u32".into());
        root.insert("f32", "f32".into());

        root
    });
}

pub(crate) fn to_wgsl_path<'o>(input: &str) -> Option<&'o str> {
    PATH_NAMES.with_borrow(|root| {
        root.get(input).map(|cow| match cow {
            Cow::Owned(s) => {
                let x = s.as_str() as *const str;
                unsafe { x.as_ref().unwrap_unchecked() }
            }
            Cow::Borrowed(s) => s,
        })
    })
}

pub(crate) fn insert_wgsl_path<T>(s: T)
where
    T: Into<Cow<'static, str>>,
{
    let s = s.into();
    PATH_NAMES.with_borrow_mut(|root| {
        root.push(s);
    });
}
