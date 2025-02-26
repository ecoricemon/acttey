//! # Conversion rules
//!
//! | Scope | Rust                        | WGSL                     |
//! | :-:   | :-:                         | :-:                      |
//! | mod   | const                       | const                    |
//! | fn    | const                       | const                    |
//! | mod   | const Override<T>           | override                 |
//! | fn    | let                         | let                      |
//! | fn    | let mut                     | var<function>, var       |
//! | mod   | static Private<T>           | var<private>             |
//! | mod   | static Workgroup<T>         | var<workgroup>           |
//! | mod   | static Uniform<T, G, B>     | var<uniform>             |
//! | mod   | static Storage<T, G, B>     | var<storage>             |
//! | mod   | static Storage<T, G, B, RW> | var<storage, read_write> |

use super::{attr::*, expr::*, path::*, traits::*, util::*};
use proc_macro2::{Punct, Spacing, TokenStream as TokenStream2};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{Ident, ItemConst, Result, Token, Type, Visibility};

#[derive(Debug)]
pub(crate) struct WgslConst {
    pub(crate) attrs: WgslAttributes,
    pub(crate) vis: Visibility,
    pub(crate) ident: Ident,
    pub(crate) ty: Type,
    pub(crate) expr: WgslExpr,
}

impl FromSyn<ItemConst> for WgslConst {
    fn from_syn(input: ItemConst) -> Result<Self> {
        // Records ident of the const.
        insert_wgsl_path(input.ident.to_string());

        Ok(Self {
            attrs: FromSyn::from_syn(input.attrs)?,
            vis: input.vis,
            ident: input.ident,
            ty: *input.ty,
            expr: WgslExpr::from_syn(*input.expr)?,
        })
    }
}

impl WgslConst {
    fn is_override(&self) -> bool {
        if let Ok(ident) = last_type_path_ident(&self.ty) {
            ident.to_string().starts_with("Override<")
        } else {
            false
        }
    }
}

impl ToTokens for WgslConst {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.attrs.to_tokens(tokens);
        self.vis.to_tokens(tokens);
        let const_token: Token![const] = Default::default();
        const_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        tokens.append(Punct::new(':', Spacing::Alone));
        self.ty.to_tokens(tokens);
        tokens.append(Punct::new('=', Spacing::Alone));
        self.expr.to_tokens(tokens);
        tokens.append(Punct::new(';', Spacing::Alone));
    }
}

impl ToWgslString for WgslConst {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        if self.is_override() {
            buf.push_str("override ");
        } else {
            buf.push_str("const ");
        }
        buf.push_str(&self.ident.to_string());

        if !self.ty.is_abstract_type() {
            buf.push(':');
            self.ty.write_wgsl_string(buf);
        }

        buf.push('=');
        self.expr.write_wgsl_string(buf);
        buf.push(';');
    }
}

impl RuntimeWgslToken for WgslConst {
    fn runtime_tokens(&self) -> TokenStream2 {
        let Self {
            ident, ty, expr, ..
        } = self;

        let attrs = self.attrs.iter().map(RuntimeWgslToken::runtime_tokens);

        let assign_ty = if !ty.is_abstract_type() {
            let ty = ty.wgsl_string();
            Some(quote! { var.ty = Some(#ty.to_owned()); })
        } else {
            None
        };

        let expr = expr.wgsl_string();

        quote! {
            {
                let mut var = my_wgsl::WgslVarDecl::new();
                var.attrs = my_wgsl::Attributes(vec![#(#attrs),*]);
                var.kind = my_wgsl::VarKind::Const;
                var.ident = stringify!(#ident).to_owned();
                #assign_ty
                var.expr = Some(#expr.to_owned());
                var
            }
        }
    }
}
