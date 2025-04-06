use super::{attr::*, externs::*, structs::*, traits::*, util::*, var::*};
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt, quote};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    ops::{Deref, DerefMut},
};
use syn::{
    Error, Expr, Ident, Item, ItemConst, Result, Token, Type, TypePath, Visibility, braced,
    parse::{Parse, ParseStream},
    parse_quote, parse2,
    spanned::Spanned,
    token::Brace,
};
use wgsl_builtin::{helper::*, prelude::*};

#[derive(Debug)]
pub(crate) struct WgslMod {
    attrs: WgslAttributes,
    vis: Visibility,
    mod_token: Token![mod],
    ident: Ident,
    brace_token: Brace,
    items: Vec<WgslItem>,
    memo: RefCell<Memo>,
}

impl WgslMod {
    fn extend_items(&mut self, items: Vec<Item>) -> Result<()> {
        // Bypasses hidden items.
        let mut input: BTreeMap<usize, Item> = items.into_iter().enumerate().collect();
        let mut output = BTreeMap::new();
        Self::bypass_hidden_items(&mut input, &mut output);

        // Imports extern values.
        self.import_extern(&input)?;

        // Converts `Const` -> `WgslConst`
        self.convert_const(&mut input, &mut output)?;

        // Converts `Struct` -> `WgslStrcut`
        self.convert_struct(&mut input, &mut output)?;

        for (i, item) in input {
            output.insert(i, WgslItem::Other(item));
        }

        self.items.extend(output.into_values());

        Ok(())
    }

    fn bypass_hidden_items(
        input: &mut BTreeMap<usize, Item>,
        output: &mut BTreeMap<usize, WgslItem>,
    ) {
        let mut bypass = Vec::new();
        for (i, item) in input.iter_mut() {
            if item.contains_attribute(ATTR_HIDE) {
                item.remove_attribute(ATTR_HIDE);
                bypass.push(*i);
            }
        }
        while let Some(i) = bypass.pop() {
            let item = WgslItem::Other(input.remove(&i).unwrap());
            output.insert(i, item);
        }
    }

    fn import_extern(&self, input: &BTreeMap<usize, Item>) -> Result<()> {
        for item in input.values() {
            let Item::Macro(m) = item else {
                continue;
            };
            // Imports extern types.
            if m.mac.path.is_ident(EXTERN_TYPE) {
                let ext = parse2::<ExternType>(m.mac.tokens.clone())?;
                self.memo.borrow_mut().insert(
                    ext.as_ident()?.clone(),
                    MemoValue::Layout {
                        rust: ext.layout(),
                        wgsl: ext.layout(),
                    },
                );
            }
            // Imports extern const integers.
            else if m.mac.path.is_ident(EXTERN_CONST) {
                let ext = parse2::<ExternConst>(m.mac.tokens.clone())?;
                let value = MemoValue::infer_const(&ext.value)?;
                self.memo
                    .borrow_mut()
                    .insert(ext.as_ident()?.clone(), value);
            }
        }

        Ok(())
    }

    fn convert_const(
        &self,
        input: &mut BTreeMap<usize, Item>,
        output: &mut BTreeMap<usize, WgslItem>,
    ) -> Result<()> {
        // A helper for searching const values in a DFS way.
        let wgsl_consts = RefCell::new(HashMap::new());
        struct Helper<'a> {
            f: &'a dyn Fn(&Helper, &Ident) -> Result<MemoValue>,
        }
        let helper = Helper {
            f: &|h, ident| {
                if let Some(value) = self.memo.borrow().get(ident) {
                    return Ok(*value);
                }
                for item in input.values() {
                    let Item::Const(c) = item else {
                        continue;
                    };
                    if &c.ident != ident {
                        continue;
                    }

                    let value = MemoValue::new_const(c, |ident| (h.f)(h, ident))?;
                    self.memo.borrow_mut().insert(ident.clone(), value);
                    wgsl_consts
                        .borrow_mut()
                        .insert(ident.clone(), WgslConst::from_syn(c.clone())?);
                    return Ok(value);
                }

                Ok(MemoValue::UnknownConst)
            },
        };

        for item in input.values() {
            if let Item::Const(c) = item {
                (helper.f)(&helper, &c.ident)?;
            }
        }

        let mut wgsl_consts = wgsl_consts.borrow_mut();
        for (i, item) in input.iter() {
            if let Item::Const(c) = item {
                let c = wgsl_consts
                    .remove(&c.ident)
                    .ok_or(Error::new(c.ident.span(), "duplicated"))?;
                output.insert(*i, WgslItem::Const(c));
            }
        }
        for (i, _) in output
            .iter()
            .filter(|(_, item)| matches!(item, WgslItem::Const(_)))
        {
            input.remove(i);
        }

        Ok(())
    }

    fn convert_struct(
        &self,
        input: &mut BTreeMap<usize, Item>,
        output: &mut BTreeMap<usize, WgslItem>,
    ) -> Result<()> {
        // A helper for searching struct layouts in a DFS way.
        let wgsl_structs = RefCell::new(HashMap::new());
        struct Helper<'a> {
            #[allow(clippy::complexity)]
            f: &'a dyn Fn(&Helper, &Ident, bool) -> Result<MemoValue>,
        }
        let helper = Helper {
            f: &|h, ident, uni| {
                if let Some(value) = self.memo.borrow().get(ident) {
                    return Ok(*value);
                }
                for item in input.values() {
                    let Item::Struct(st) = item else {
                        continue;
                    };
                    if &st.ident != ident {
                        continue;
                    }

                    let find_layout = |ident: &Ident| {
                        if let MemoValue::Layout { rust, wgsl } = (h.f)(h, ident, uni)? {
                            Ok((rust, wgsl))
                        } else {
                            Err(Error::new(ident.span(), "could not find layout"))
                        }
                    };
                    let find_len = |ident: &Ident| match (h.f)(h, ident, uni)? {
                        MemoValue::Int(v) => Ok(Some(v as usize)),
                        MemoValue::UnknownConst => Ok(None),
                        _ => Err(Error::new(ident.span(), "not an integer")),
                    };
                    let st = WgslStruct::new(st, uni, find_layout, find_len)?;
                    let value = MemoValue::Layout {
                        rust: st.layout(),
                        wgsl: st.layout(),
                    };
                    self.memo.borrow_mut().insert(ident.clone(), value);
                    wgsl_structs.borrow_mut().insert(ident.clone(), st);
                    return Ok(value);
                }

                if let Some(s) = self.find_similar_type_ident(ident) {
                    let reason = format!(
                        "unknown symbol in this module. did you mean `{s}`? or consider using `{EXTERN_TYPE}!`"
                    );
                    Err(Error::new(ident.span(), reason))
                } else {
                    let reason = const_format::concatcp!(
                        "unknown symbol in this module. consider using `",
                        EXTERN_TYPE,
                        "!`"
                    );
                    Err(Error::new(ident.span(), reason))
                }
            },
        };

        // Processes uniform structs and non-uniform structs.
        for item in input.values() {
            if let Item::Struct(st) = item {
                if st.contains_attribute(ATTR_UNIFORM) {
                    (helper.f)(&helper, &st.ident, true)?;
                }
            }
        }
        for item in input.values() {
            if let Item::Struct(st) = item {
                if !st.contains_attribute(ATTR_UNIFORM) {
                    (helper.f)(&helper, &st.ident, false)?;
                }
            }
        }

        let mut wgsl_structs = wgsl_structs.borrow_mut();
        for (i, item) in input.iter() {
            if let Item::Struct(st) = item {
                let st = wgsl_structs
                    .remove(&st.ident)
                    .ok_or(Error::new(st.ident.span(), "duplicated"))?;
                output.insert(*i, WgslItem::Struct(st));
            }
        }
        for (i, _) in output
            .iter()
            .filter(|(_, item)| matches!(item, WgslItem::Struct(_)))
        {
            input.remove(i);
        }

        Ok(())
    }

    fn find_similar_type_ident(&self, target: &Ident) -> Option<String> {
        let target = target.to_string();
        let mut max_score = 0.0;
        let mut max_ident = String::new();

        for (ident, _) in self
            .memo
            .borrow()
            .iter()
            .filter(|(_, v)| matches!(v, MemoValue::Layout { .. }))
        {
            let known = ident.to_string();
            let score = Self::calc_similarity(&target, &known);
            if score > max_score {
                max_score = score;
                max_ident = known;
            }
        }

        (max_score > 0.5).then_some(max_ident)
    }

    fn calc_similarity(a: &str, b: &str) -> f32 {
        let mut a_set = [0_i32; 128];
        for c in a
            .chars()
            .filter_map(|c| c.is_ascii_alphanumeric().then_some(c.to_ascii_lowercase()))
        {
            a_set[c as usize] += 1;
        }

        let mut same = 0;
        let mut total = 0;

        for c in b
            .chars()
            .filter_map(|c| c.is_ascii_alphanumeric().then_some(c.to_ascii_lowercase()))
        {
            if a_set[c as usize] > 0 {
                same += 1;
                total += 1;
            } else {
                total += 2;
            }
            a_set[c as usize] -= 1;
        }

        same as f32 / total as f32
    }
}

impl Parse for WgslMod {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(WgslAttributes::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let mod_token: Token![mod] = input.parse()?;
        let ident: Ident = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        attrs.extend_by_parsing_inner(&content)?;

        let mut items: Vec<Item> = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        let mut this = Self {
            attrs,
            vis,
            mod_token,
            ident,
            brace_token,
            items: Vec::new(),
            memo: RefCell::new(Memo::new()),
        };

        this.extend_items(items)?;

        Ok(this)
    }
}

impl ToTokens for WgslMod {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.attrs.outer());
        self.vis.to_tokens(tokens);
        self.mod_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            tokens.append_all(self.attrs.inner());
            tokens.append_all(&self.items);
            tokens.append_all(self.runtime_tokens());
        });
    }
}

impl ToWgslString for WgslMod {
    fn write_wgsl_string(&self, buf: &mut String) {
        for item in &self.items {
            match item {
                WgslItem::Struct(st) => st.write_wgsl_string(buf),
                WgslItem::Const(c) => c.write_wgsl_string(buf),
                _ => {}
            }
        }
    }
}

impl RuntimeWgslToken for WgslMod {
    fn runtime_tokens(&self) -> TokenStream2 {
        let push_items = self.items.iter().filter_map(|item| match item {
            WgslItem::Struct(st) => {
                let ident = &st.ident;
                Some(quote! {
                    module.entries.push(my_wgsl::WgslEntry::Struct(
                        my_wgsl::WgslStruct::of::<#ident>()
                    ));
                })
            }
            WgslItem::Const(c) => {
                let var = c.runtime_tokens();
                Some(quote! {
                    module.entries.push(my_wgsl::WgslEntry::GlobalVariable(
                        #var
                    ));
                })
            }
            WgslItem::Other(_) => None,
        });

        let wgsl_code = self.wgsl_string();

        quote! {
            // `pub` because the helper struct must be shown to outside.
            pub struct Module;

            impl my_wgsl::BeWgslModule for Module {
                fn be_module() -> my_wgsl::WgslModule {
                    let mut module = my_wgsl::WgslModule::new();
                    #(#push_items)*
                    module
                }
            }

            impl Module {
                pub const WGSL: &str = #wgsl_code;
            }
        }
    }
}

#[derive(Debug)]
pub(crate) enum WgslItem {
    Struct(WgslStruct),
    Const(WgslConst),
    Other(Item),
}

impl ToTokens for WgslItem {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Struct(st) => st.to_tokens(tokens),
            Self::Const(c) => c.to_tokens(tokens),
            Self::Other(o) => o.to_tokens(tokens),
        }
    }
}

#[derive(Debug)]
struct Memo(HashMap<Ident, MemoValue>);

impl Memo {
    fn new() -> Self {
        let mut this = Self(HashMap::new());
        this.extend_default_layouts();
        this
    }

    fn extend_default_layouts(&mut self) {
        const IS_SIZED: bool = true;

        macro_rules! entry {
            ($ty:ident) => {{
                let key = parse_quote!($ty);

                let rust_layout =
                    LayoutExt::new(size_of::<$ty>(), align_of::<$ty>(), IS_SIZED).unwrap();
                let wgsl_layout =
                    LayoutExt::new($ty::wgsl_size(), $ty::wgsl_align(), IS_SIZED).unwrap();

                let value = MemoValue::Layout {
                    rust: rust_layout,
                    wgsl: wgsl_layout,
                };

                (key, value)
            }};
        }

        self.extend([
            // NOTE: bool is added since 2025 draft, but may not be allowed in
            // browsers yet.
            // 2024: https://www.w3.org/TR/2024/CR-WGSL-20241219/#alignment-and-size
            // 2025: https://www.w3.org/TR/2025/CRD-WGSL-20250116/#alignment-and-size
            entry!(Bool),
            entry!(i32),
            entry!(u32),
            entry!(f32),
            entry!(Vec2i),
            entry!(Vec3i),
            entry!(Vec4i),
            entry!(Vec2u),
            entry!(Vec3u),
            entry!(Vec4u),
            entry!(Vec2f),
            entry!(Vec3f),
            entry!(Vec4f),
            entry!(WideVec2i),
            entry!(WideVec3i),
            entry!(WideVec4i),
            entry!(WideVec2u),
            entry!(WideVec3u),
            entry!(WideVec4u),
            entry!(WideVec2f),
            entry!(WideVec3f),
            entry!(WideVec4f),
            entry!(Mat2x2f),
            entry!(Mat2x3f),
            entry!(Mat2x4f),
            entry!(Mat3x2f),
            entry!(Mat3x3f),
            entry!(Mat3x4f),
            entry!(Mat4x2f),
            entry!(Mat4x3f),
            entry!(Mat4x4f),
        ])
    }
}

impl Deref for Memo {
    type Target = HashMap<Ident, MemoValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Memo {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, Copy)]
enum MemoValue {
    /// Layout of a type.
    ///
    /// We use this memo when we find a layout of a type.
    Layout { rust: LayoutExt, wgsl: LayoutExt },

    /// Abstract-int in a const expression.
    ///
    /// If an abstract-int is used in structs, it should be evaluated at macro
    /// expansion time due to determining array layouts.
    Int(i64),

    /// Abstract-float in a const expression.
    ///
    /// We may need to evaluate an abstract-float like abstract-int.
    Float(f64),

    /// Not evaluated constant.
    ///
    /// It's fine to not evaluate if the value is not used in structs.
    UnknownConst,

    /// No memoization.
    Blank,
}

impl MemoValue {
    pub(crate) fn new_const<F>(c: &ItemConst, find: F) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<MemoValue>,
    {
        match &*c.ty {
            Type::Path(type_path) => Self::from_type_path(c, type_path, find),
            _ => Ok(Self::Blank),
        }
    }

    fn from_type_path<F>(c: &ItemConst, path: &TypePath, mut find: F) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<MemoValue>,
    {
        match last_path_ident(&path.path)?.to_string().as_str() {
            "i8" | "u8" | "i32" | "u32" | "i64" | "u64" | "isize" | "usize" => {
                let eval = c.expr.evaluate(&mut |ident| {
                    match find(ident)? {
                        MemoValue::Int(v) => Ok(Some(v)),
                        // Abtract-float may not be able to become int? But fine.
                        MemoValue::Float(v) => Ok(Some(v as i64)),
                        MemoValue::UnknownConst => Ok(None),
                        _ => Err(Error::new(
                            ident.span(),
                            "expected integer or floating number",
                        )),
                    }
                })?;
                if let Some(eval) = eval {
                    Ok(Self::Int(eval))
                } else {
                    Ok(Self::UnknownConst)
                }
            }
            "f32" | "f64" => {
                let eval = c.expr.evaluate(&mut |ident| match find(ident)? {
                    MemoValue::Int(v) => Ok(Some(v as f64)),
                    MemoValue::Float(v) => Ok(Some(v)),
                    MemoValue::UnknownConst => Ok(None),
                    _ => Err(Error::new(
                        ident.span(),
                        "expected integer or floating number",
                    )),
                })?;
                if let Some(eval) = eval {
                    Ok(Self::Float(eval))
                } else {
                    Ok(Self::UnknownConst)
                }
            }
            _ => Ok(Self::Blank),
        }
    }

    pub(crate) fn infer_const(expr: &Expr) -> Result<Self> {
        if let Ok(value) = expr_to_number::<i64>(expr) {
            Ok(Self::Int(value))
        } else if let Ok(value) = expr_to_number::<f64>(expr) {
            Ok(Self::Float(value))
        } else {
            Err(Error::new(expr.span(), "could not infer"))
        }
    }
}
