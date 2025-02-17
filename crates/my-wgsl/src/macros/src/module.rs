use super::{structs::*, util::*};
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens, TokenStreamExt};
use std::{alloc::Layout, cell::RefCell, collections::HashMap};
use syn::{parse_quote, Attribute, Error, Ident, Item, ItemMod, Result, Visibility};

#[derive(Debug)]
pub(crate) struct WgslMod {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    items: Vec<WgslItem>,
}

impl WgslMod {
    pub(crate) fn new(module: &ItemMod) -> Result<Self> {
        let items = if let Some((_, items)) = &module.content {
            Self::create_wgsl_items(items)?
        } else {
            Vec::new()
        };

        Ok(Self {
            attrs: module.attrs.clone(),
            vis: module.vis.clone(),
            ident: module.ident.clone(),
            items,
        })
    }

    fn create_wgsl_items(items: &[Item]) -> Result<Vec<WgslItem>> {
        let layouts = RefCell::new(Self::default_layouts());
        let wgsl_items = RefCell::new(HashMap::new());

        struct Helper<'a> {
            #[allow(clippy::complexity)]
            f: &'a dyn Fn(&Helper, &Ident, bool) -> Result<(LayoutExt, LayoutExt)>,
        }
        let helper = Helper {
            f: &|h, ident, uni| {
                if let Some(layout) = layouts.borrow().get(ident) {
                    return Ok(*layout);
                }
                for item in items {
                    let Item::Struct(st) = item else {
                        continue;
                    };
                    if &st.ident != ident {
                        continue;
                    }

                    let wgsl_st = WgslStruct::new(st, uni, |id| (h.f)(h, id, uni))?;
                    let layout = (wgsl_st.layout(), wgsl_st.layout());
                    layouts.borrow_mut().insert(ident.clone(), layout);
                    wgsl_items.borrow_mut().insert(ident.clone(), wgsl_st);

                    return Ok(layout);
                }

                let mut reason = format!("`{ident}` is unknown in this module");
                if let Some(s) = Self::find_similar_ident(ident, &layouts.borrow()) {
                    let extra_info = format!(". did you mean `{s}`?");
                    reason.push_str(&extra_info);
                }
                Err(Error::new(ident.span(), reason))
            },
        };

        // Processes uniform structs first.
        for item in items {
            if let Item::Struct(st) = item {
                if st.contains_attribute(ATTR_UNIFORM) {
                    (helper.f)(&helper, &st.ident, true)?;
                }
            }
        }
        for item in items {
            if let Item::Struct(st) = item {
                if !st.contains_attribute(ATTR_UNIFORM) {
                    (helper.f)(&helper, &st.ident, false)?;
                }
            }
        }

        let mut res = Vec::new();
        let mut wgsl_items = wgsl_items.borrow_mut();
        for item in items {
            match item {
                Item::Struct(st) => res.push(WgslItem::Struct(
                    wgsl_items
                        .remove(&st.ident)
                        .ok_or(Error::new(st.ident.span(), "duplicated"))?,
                )),
                o => res.push(WgslItem::Other(o.clone())),
            }
        }
        Ok(res)
    }

    // Returns type ident -> (Rust layout, WGSL layout) map.
    fn default_layouts() -> HashMap<Ident, (LayoutExt, LayoutExt)> {
        const SIZED: bool = true;

        macro_rules! entry {
            ($key:ident, $ty:ty, $size:expr, $align:expr) => {{
                let key = parse_quote!($key);
                let value = (LayoutExt::new(Layout::new::<$ty>(), SIZED), unsafe {
                    LayoutExt::new(Layout::from_size_align_unchecked($size, $align), SIZED)
                });
                (key, value)
            }};
        }

        HashMap::from([
            // NOTE: bool is added since 2025 draft, but may not be allowed in
            // browsers yet.
            // 2024: https://www.w3.org/TR/2024/CR-WGSL-20241219/#alignment-and-size
            // 2025: https://www.w3.org/TR/2025/CRD-WGSL-20250116/#alignment-and-size
            entry!(Bool, super::Bool, 4, 4),
            entry!(i32, i32, 4, 4),
            entry!(u32, u32, 4, 4),
            entry!(f32, f32, 4, 4),
            entry!(Vec2i, super::Vec2i, 8, 8),
            entry!(Vec3i, super::Vec3i, 12, 16),
            entry!(Vec4i, super::Vec4i, 16, 16),
            entry!(Vec2u, super::Vec2u, 8, 8),
            entry!(Vec3u, super::Vec3u, 12, 16),
            entry!(Vec4u, super::Vec4u, 16, 16),
            entry!(Vec2f, super::Vec2f, 8, 8),
            entry!(Vec3f, super::Vec3f, 12, 16),
            entry!(Vec4f, super::Vec4f, 16, 16),
            entry!(WideVec2i, super::WideVec2i, 8, 8),
            entry!(WideVec3i, super::WideVec3i, 16, 16),
            entry!(WideVec4i, super::WideVec4i, 16, 16),
            entry!(WideVec2u, super::WideVec2u, 8, 8),
            entry!(WideVec3u, super::WideVec3u, 16, 16),
            entry!(WideVec4u, super::WideVec4u, 16, 16),
            entry!(WideVec2f, super::WideVec2f, 8, 8),
            entry!(WideVec3f, super::WideVec3f, 16, 16),
            entry!(WideVec4f, super::WideVec4f, 16, 16),
            entry!(Mat2x2f, super::Mat2x2f, 16, 8),
            entry!(Mat2x3f, super::Mat2x3f, 32, 16),
            entry!(Mat2x4f, super::Mat2x4f, 32, 16),
            entry!(Mat3x2f, super::Mat3x2f, 24, 8),
            entry!(Mat3x3f, super::Mat3x3f, 48, 16),
            entry!(Mat3x4f, super::Mat3x4f, 48, 16),
            entry!(Mat4x2f, super::Mat4x2f, 32, 8),
            entry!(Mat4x3f, super::Mat4x3f, 64, 16),
            entry!(Mat4x4f, super::Mat4x4f, 64, 16),
        ])
    }

    fn find_similar_ident(
        target: &Ident,
        layouts: &HashMap<Ident, (LayoutExt, LayoutExt)>,
    ) -> Option<String> {
        let target = target.to_string();
        let mut max_score = 0.0;
        let mut max_ident = String::new();
        for ident in layouts.keys() {
            let known = ident.to_string();
            let score = calc_similarity(&target, &known);
            if score > max_score {
                max_score = score;
                max_ident = known;
            }
        }

        return (max_score > 0.5).then_some(max_ident);

        // === Inernal helper functions ===

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

    fn to_wgsl_code_tokens(&self) -> TokenStream2 {
        let push_items = self.items.iter().filter_map(|item| match item {
            WgslItem::Struct(st) => {
                let st_ident = &st.ident;
                Some(quote! {
                     module.entries.push(my_wgsl::WgslEntry::Struct(
                        WgslStruct::of::<#st_ident>()
                     ));
                })
            }
            WgslItem::Other(_) => None,
        });

        quote! {
            // `pub` because the helper struct must be shown to outside.
            pub struct Module;
            impl my_wgsl::BeWgslModule for Module {
                fn be_module() -> my_wgsl::WgslModule {
                    let mut module = WgslModule::new();

                    #(#push_items)*

                    module
                }
            }
        }
    }
}

impl ToTokens for WgslMod {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            attrs,
            vis,
            ident,
            items,
        } = self;

        let decl_module_helper = self.to_wgsl_code_tokens();

        tokens.append_all(quote! {
            #(#attrs)*
            #vis mod #ident {
                #(#items)*
                #decl_module_helper
            }
        });
    }
}

#[derive(Debug)]
pub(crate) enum WgslItem {
    Struct(WgslStruct),
    Other(Item),
}

impl ToTokens for WgslItem {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Struct(st) => st.to_tokens(tokens),
            Self::Other(o) => o.to_tokens(tokens),
        }
    }
}
