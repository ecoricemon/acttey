use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::ToTokens;
use std::{fs, hash, marker::PhantomPinned, path::PathBuf, pin::Pin};
use syn::{Error, Result};
use syn_locator::{Locate, LocateEntry, LocateGroup, Location};

// === SmFile ===

#[derive(Clone, Debug)]
pub struct SmFile {
    pub file: syn::File,
    pub abs_path: PathBuf,
    _pin: PhantomPinned,
}

impl SmFile {
    pub fn from_path(abs_path: PathBuf) -> Result<Pin<Box<Self>>> {
        let code = fs::read_to_string(&abs_path).map_err(|e| Error::new(Span::call_site(), e))?;

        let this = Box::pin(Self {
            file: syn::parse_file(&code)?,
            abs_path: abs_path.clone(),
            _pin: PhantomPinned,
        });

        let fpath = abs_path
            .as_os_str()
            .to_str()
            .unwrap_or_else(|| panic!("{abs_path:?} contains non UTF-8 charactor"));

        this.as_ref().locate_as_entry(fpath, code.as_str());

        Ok(this)
    }
}

impl Locate for SmFile {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        self.file.locate(file_path, code, offset)
    }
}

impl ToTokens for SmFile {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.file.to_tokens(tokens);
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
