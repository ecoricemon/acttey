use super::{attr::SmAttributes, item::SmItems};
use crate::traits::FromSyn;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, TokenStreamExt};
use std::{
    fs, hash,
    marker::PhantomPinned,
    path::{Path as StdPath, PathBuf},
};
use syn::{
    File, LitStr, Result, Token,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

// === ImportFilePaths ===

#[derive(Debug)]
pub struct ImportFilePaths {
    pub paths: Punctuated<LitStr, Token![,]>,
}

impl ImportFilePaths {
    pub fn iter(&self) -> impl Iterator<Item = PathBuf> + Clone {
        self.paths
            .iter()
            .map(|path| StdPath::new(&path.value()).to_path_buf())
    }
}

impl Parse for ImportFilePaths {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            paths: Punctuated::parse_separated_nonempty(input)?,
        })
    }
}

// === SmFile ===

#[derive(Clone, Debug)]
pub struct SmFile {
    pub attrs: SmAttributes,
    pub items: SmItems,
    pub abs_path: PathBuf,
    _pin: PhantomPinned,
}

impl SmFile {
    pub fn from_path(fpath: PathBuf) -> Result<Self> {
        let code = fs::read_to_string(&fpath).map_err(|e| syn::Error::new(Span::call_site(), e))?;
        let mut this = syn::parse_str::<Self>(&code)?;
        this.abs_path = fpath;
        Ok(this)
    }
}

impl FromSyn<File> for SmFile {
    fn from_syn(input: File) -> Result<Self> {
        Ok(Self {
            attrs: FromSyn::from_syn(input.attrs)?,
            items: FromSyn::from_syn(input.items)?,
            abs_path: PathBuf::new(),
            _pin: PhantomPinned,
        })
    }
}

impl Parse for SmFile {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            attrs: input.call(SmAttributes::parse_inner)?,
            items: input.parse()?,
            abs_path: PathBuf::new(),
            _pin: PhantomPinned,
        })
    }
}

impl ToTokens for SmFile {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.inner());
        self.items.to_tokens(tokens);
    }
}

impl PartialEq for SmFile {
    fn eq(&self, other: &Self) -> bool {
        self.abs_path == other.abs_path
    }
}

impl Eq for SmFile {}

impl hash::Hash for SmFile {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.abs_path.hash(state)
    }
}
