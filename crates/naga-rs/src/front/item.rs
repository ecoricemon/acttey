use super::attr::SmAttributes;
use crate::traits::{FromSyn, SmAttributeHelper};
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt};
use std::{
    iter,
    ops::{Deref, DerefMut},
};
use syn::{
    Expr, Field, Fields, FieldsNamed, FieldsUnnamed, Generics, Ident, Item, ItemConst, ItemFn,
    ItemMod, ItemStruct, ItemType, ItemUse, Result, Token, Type, Visibility, braced,
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::{self, Pair, Punctuated},
    token::{Brace, Paren},
};

// === SmItems ===

#[derive(Clone, Debug)]
pub struct SmItems(pub Vec<SmItem>);

impl FromSyn<Vec<Item>> for SmItems {
    fn from_syn(input: Vec<Item>) -> Result<Self> {
        let mut items = Vec::new();
        for item in input {
            items.push(FromSyn::from_syn(item)?);
        }
        Ok(Self(items))
    }
}

impl Parse for SmItems {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut items = Vec::new();
        while !input.is_empty() {
            items.push(input.parse()?);
        }
        Ok(Self(items))
    }
}

impl ToTokens for SmItems {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(&self.0);
    }
}

impl Deref for SmItems {
    type Target = Vec<SmItem>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SmItems {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl IntoIterator for SmItems {
    type Item = SmItem;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a SmItems {
    type Item = &'a SmItem;
    type IntoIter = std::slice::Iter<'a, SmItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

// === SmItem ===

#[derive(Clone, Debug)]
pub enum SmItem {
    Const(SmItemConst), // Custom
    Fn(ItemFn),
    Mod(SmItemMod),       // Custom
    Struct(SmItemStruct), // Custom
    Type(ItemType),
    Use(ItemUse),
    Other(Item),
}

impl FromSyn<Item> for SmItem {
    fn from_syn(input: Item) -> Result<Self> {
        Ok(match input {
            Item::Const(v) => Self::Const(FromSyn::from_syn(v)?),
            Item::Fn(v) => Self::Fn(v),
            Item::Mod(v) => Self::Mod(FromSyn::from_syn(v)?),
            Item::Struct(v) => Self::Struct(FromSyn::from_syn(v)?),
            Item::Type(v) => Self::Type(v),
            Item::Use(v) => Self::Use(v),
            o => Self::Other(o),
        })
    }
}

impl Parse for SmItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.fork();
        let _attrs = ahead.call(SmAttributes::parse_outer)?;
        let _vis: Visibility = ahead.parse()?;

        // Parsing for Const(SmItemConst).
        if ahead.peek(Token![const]) {
            Ok(Self::Const(input.parse()?))
        }
        // Parsing for Mod(SmItemMod).
        else if ahead.peek(Token![mod]) {
            Ok(Self::Mod(input.parse()?))
        }
        // Parsing for Struct(SmItemStruct).
        else if ahead.peek(Token![struct]) {
            Ok(Self::Struct(input.parse()?))
        }
        // Parsing for non-custom variants.
        else {
            let other: Item = input.parse()?;
            FromSyn::from_syn(other)
        }
    }
}

impl ToTokens for SmItem {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Const(v) => { /* TODO */ }
            Self::Fn(v) => { /* TODO */ }
            Self::Mod(v) => { /* TODO */ }
            Self::Struct(v) => { /* TODO */ }
            Self::Type(v) => { /* TODO */ }
            Self::Use(v) => { /* TODO */ }
            Self::Other(v) => v.to_tokens(tokens),
        }
    }
}

// === SmItemConst ===

#[derive(Clone, Debug)]
pub struct SmItemConst {
    pub attrs: SmAttributes,
    pub vis: Visibility,
    pub const_token: Token![const],
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Box<Type>,
    pub eq_token: Token![=],
    pub expr: Expr,
    pub semi_token: Token![;],
}

impl FromSyn<ItemConst> for SmItemConst {
    fn from_syn(input: ItemConst) -> Result<Self> {
        Ok(Self {
            attrs: FromSyn::from_syn(input.attrs)?,
            vis: input.vis,
            const_token: input.const_token,
            ident: input.ident,
            colon_token: input.colon_token,
            ty: input.ty,
            eq_token: input.eq_token,
            expr: *input.expr,
            semi_token: input.semi_token,
        })
    }
}

impl Parse for SmItemConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(SmAttributes::parse_outer)?;
        let vis = input.parse()?;
        let const_token = input.parse()?;

        let lookahead = input.lookahead1();
        let ident = if lookahead.peek(Ident) || lookahead.peek(Token![_]) {
            input.call(Ident::parse_any)?
        } else {
            return Err(lookahead.error());
        };

        Ok(Self {
            attrs,
            vis,
            const_token,
            ident,
            colon_token: input.parse()?,
            ty: input.parse()?,
            eq_token: input.parse()?,
            expr: input.parse()?,
            semi_token: input.parse()?,
        })
    }
}

impl ToTokens for SmItemConst {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.outer());
        self.vis.to_tokens(tokens);
        self.const_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        self.ty.to_tokens(tokens);
        self.eq_token.to_tokens(tokens);
        self.expr.to_tokens(tokens);
        self.semi_token.to_tokens(tokens);
    }
}

// === SmItemMod ===

#[derive(Clone, Debug)]
pub struct SmItemMod {
    pub attrs: SmAttributes,
    pub vis: Visibility,
    pub mod_token: Token![mod],
    pub ident: Ident,
    pub content: Option<(Brace, SmItems)>,
    pub semi_token: Option<Token![;]>,
}

impl SmAttributeHelper for SmItemMod {
    fn get_attributes(&self) -> Option<&SmAttributes> {
        Some(&self.attrs)
    }

    fn get_mut_attributes(&mut self) -> Option<&mut SmAttributes> {
        Some(&mut self.attrs)
    }
}

impl FromSyn<ItemMod> for SmItemMod {
    fn from_syn(input: ItemMod) -> Result<Self> {
        let content = if let Some((brace_token, items)) = input.content {
            Some((brace_token, FromSyn::from_syn(items)?))
        } else {
            None
        };

        Ok(Self {
            attrs: FromSyn::from_syn(input.attrs)?,
            vis: input.vis,
            mod_token: input.mod_token,
            ident: input.ident,
            content,
            semi_token: input.semi,
        })
    }
}

impl Parse for SmItemMod {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(SmAttributes::parse_outer)?;
        let vis = input.parse()?;
        let mod_token = input.parse()?;
        let ident: Ident = input.parse()?;

        let lookahead = input.lookahead1();
        if lookahead.peek(Token![;]) {
            Ok(Self {
                attrs,
                vis,
                mod_token,
                ident,
                content: None,
                semi_token: Some(input.parse()?),
            })
        } else if lookahead.peek(Brace) {
            let content;
            let brace_token = braced!(content in input);
            attrs.extend_by_parsing_inner(&content)?;
            let items = content.parse()?;

            Ok(Self {
                attrs,
                vis,
                mod_token,
                ident,
                content: Some((brace_token, items)),
                semi_token: None,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for SmItemMod {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.outer());
        self.vis.to_tokens(tokens);
        self.mod_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        if let Some((brace_token, items)) = &self.content {
            brace_token.surround(tokens, |tokens| {
                tokens.append_all(self.attrs.inner());
                items.to_tokens(tokens);
            });
        } else {
            self.semi_token.as_ref().unwrap().to_tokens(tokens);
        }
    }
}

// === SmItemStruct ===

#[derive(Clone, Debug)]
pub struct SmItemStruct {
    pub attrs: SmAttributes,
    pub vis: Visibility,
    pub struct_token: Token![struct],
    pub ident: Ident,
    pub generics: Generics,
    pub fields: SmFields,
    pub semi_token: Option<Token![;]>,
}

impl SmAttributeHelper for SmItemStruct {
    fn get_attributes(&self) -> Option<&SmAttributes> {
        Some(&self.attrs)
    }

    fn get_mut_attributes(&mut self) -> Option<&mut SmAttributes> {
        Some(&mut self.attrs)
    }
}

impl FromSyn<ItemStruct> for SmItemStruct {
    fn from_syn(input: ItemStruct) -> Result<Self> {
        Ok(Self {
            attrs: FromSyn::from_syn(input.attrs)?,
            vis: input.vis,
            struct_token: input.struct_token,
            ident: input.ident,
            generics: input.generics,
            fields: FromSyn::from_syn(input.fields)?,
            semi_token: input.semi_token,
        })
    }
}

impl Parse for SmItemStruct {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(SmAttributes::parse_outer)?;
        let vis = input.parse()?;
        let struct_token = input.parse()?;
        let ident = input.parse()?;

        // Unit           : struct S;
        // Unnamed fields : struct S<T>(T) where T: U;
        //                : struct S<T>(T);
        // Named fields   : struct S<T> where T { t: T }
        //                : struct S<T> { t: T }

        let mut generics: Generics = input.parse()?;
        let mut fields = SmFields::Unit;
        let mut semi_token = None;
        let mut lookahead = input.lookahead1();

        if lookahead.peek(Token![;]) {
            semi_token = Some(input.parse()?);
        } else if lookahead.peek(Paren) {
            fields = SmFields::Unnamed(input.parse()?);

            lookahead = input.lookahead1();
            if lookahead.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
                semi_token = Some(input.parse()?);
            } else if lookahead.peek(Token![;]) {
                semi_token = Some(input.parse()?);
            } else {
                return Err(lookahead.error());
            }
        } else if lookahead.peek(Token![where]) {
            generics.where_clause = Some(input.parse()?);
            fields = SmFields::Named(input.parse()?);
        } else if lookahead.peek(Brace) {
            fields = SmFields::Named(input.parse()?);
        } else {
            return Err(lookahead.error());
        }

        Ok(Self {
            attrs,
            vis,
            struct_token,
            ident,
            generics,
            fields,
            semi_token,
        })
    }
}

impl ToTokens for SmItemStruct {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.outer());
        self.vis.to_tokens(tokens);
        self.struct_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.generics.to_tokens(tokens);
        match &self.fields {
            SmFields::Named(fields) => {
                self.generics.where_clause.to_tokens(tokens);
                fields.to_tokens(tokens);
            }
            SmFields::Unnamed(fields) => {
                fields.to_tokens(tokens);
                self.generics.where_clause.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
            }
            SmFields::Unit => {
                self.generics.where_clause.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
            }
        }
    }
}

// === SmFields ===

#[derive(Clone, Debug)]
pub enum SmFields {
    Named(SmFieldsNamed),
    Unnamed(SmFieldsUnnamed),
    Unit,
}

impl SmFields {
    pub fn iter(&self) -> punctuated::Iter<'_, SmField> {
        match self {
            Self::Named(v) => v.named.iter(),
            Self::Unnamed(v) => v.unnamed.iter(),
            Self::Unit => {
                thread_local! {
                    static EMPTY: Punctuated<SmField, Token![,]> = Punctuated::new();
                }

                let ptr: *const Punctuated<SmField, Token![,]> = EMPTY.with(|v| v as *const _);
                let v = unsafe { ptr.as_ref().unwrap_unchecked() };
                v.iter()
            }
        }
    }
}

impl FromSyn<Fields> for SmFields {
    fn from_syn(input: Fields) -> Result<Self> {
        Ok(match input {
            Fields::Named(v) => Self::Named(FromSyn::from_syn(v)?),
            Fields::Unnamed(v) => Self::Unnamed(FromSyn::from_syn(v)?),
            Fields::Unit => Self::Unit,
        })
    }
}

impl ToTokens for SmFields {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Named(v) => v.to_tokens(tokens),
            Self::Unnamed(v) => v.to_tokens(tokens),
            Self::Unit => {}
        }
    }
}

impl<'a> IntoIterator for &'a SmFields {
    type Item = &'a SmField;
    type IntoIter = punctuated::Iter<'a, SmField>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

// === SmFieldsNamed ===

#[derive(Debug, Clone)]
pub struct SmFieldsNamed {
    pub brace_token: Brace,
    pub named: Punctuated<SmField, Token![,]>,
}

impl FromSyn<FieldsNamed> for SmFieldsNamed {
    fn from_syn(input: FieldsNamed) -> Result<Self> {
        let mut named = Punctuated::new();
        for pair in input.named.into_pairs() {
            let new_pair = match pair {
                Pair::Punctuated(v, p) => Pair::Punctuated(FromSyn::from_syn(v)?, p),
                Pair::End(v) => Pair::End(FromSyn::from_syn(v)?),
            };
            named.extend(iter::once(new_pair));
        }

        Ok(Self {
            brace_token: input.brace_token,
            named,
        })
    }
}

impl Parse for SmFieldsNamed {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            brace_token: braced!(content in input),
            named: content.parse_terminated(SmField::parse_named, Token![,])?,
        })
    }
}

impl ToTokens for SmFieldsNamed {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.brace_token.surround(tokens, |tokens| {
            self.named.to_tokens(tokens);
        });
    }
}

// === SmFieldsUnnamed ===

#[derive(Debug, Clone)]
pub struct SmFieldsUnnamed {
    pub paren_token: Paren,
    pub unnamed: Punctuated<SmField, Token![,]>,
}

impl FromSyn<FieldsUnnamed> for SmFieldsUnnamed {
    fn from_syn(input: FieldsUnnamed) -> Result<Self> {
        let mut unnamed = Punctuated::new();
        for pair in input.unnamed.into_pairs() {
            let new_pair = match pair {
                Pair::Punctuated(v, p) => Pair::Punctuated(FromSyn::from_syn(v)?, p),
                Pair::End(v) => Pair::End(FromSyn::from_syn(v)?),
            };
            unnamed.extend(iter::once(new_pair));
        }

        Ok(Self {
            paren_token: input.paren_token,
            unnamed,
        })
    }
}

impl Parse for SmFieldsUnnamed {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            paren_token: parenthesized!(content in input),
            unnamed: content.parse_terminated(SmField::parse_unnamed, Token![,])?,
        })
    }
}

impl ToTokens for SmFieldsUnnamed {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.paren_token.surround(tokens, |tokens| {
            self.unnamed.to_tokens(tokens);
        });
    }
}

// === SmField ===

#[derive(Debug, Clone)]
pub struct SmField {
    pub attrs: SmAttributes,
    pub vis: Visibility,
    pub ident: Option<Ident>,
    pub colon_token: Option<Token![:]>,
    pub ty: Type,
}

impl SmField {
    pub fn parse_named(input: ParseStream) -> Result<Self> {
        Ok(Self {
            attrs: input.call(SmAttributes::parse_outer)?,
            vis: input.parse()?,
            ident: Some(if input.peek(Token![_]) {
                input.call(Ident::parse_any)
            } else {
                input.parse()
            }?),
            colon_token: input.parse()?,
            ty: input.parse()?,
        })
    }

    pub fn parse_unnamed(input: ParseStream) -> Result<Self> {
        Ok(Self {
            attrs: input.call(SmAttributes::parse_outer)?,
            vis: input.parse()?,
            ident: None,
            colon_token: None,
            ty: input.parse()?,
        })
    }
}

impl SmAttributeHelper for SmField {
    fn get_attributes(&self) -> Option<&SmAttributes> {
        Some(&self.attrs)
    }

    fn get_mut_attributes(&mut self) -> Option<&mut SmAttributes> {
        Some(&mut self.attrs)
    }
}

impl FromSyn<Field> for SmField {
    fn from_syn(input: Field) -> Result<Self> {
        Ok(Self {
            attrs: FromSyn::from_syn(input.attrs)?,
            vis: input.vis,
            ident: input.ident,
            colon_token: input.colon_token,
            ty: input.ty,
        })
    }
}

impl ToTokens for SmField {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.outer());
        self.vis.to_tokens(tokens);
        if let Some(ident) = &self.ident {
            ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
        }
        self.ty.to_tokens(tokens);
    }
}
