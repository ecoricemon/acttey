use super::{attr::*, path::*, traits::*, util::*};
use proc_macro2::{Punct, Spacing, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use syn::{
    parse_quote, spanned::Spanned, Attribute, Error, Field, Fields, Ident, Index, ItemStruct,
    Result, Type, Visibility,
};
use wgsl_builtin::{helper::*, prelude::*};

/// Ident prefix for Rust padding fields.
pub(crate) const PAD_PREFIX: &str = "__pad";

#[derive(Debug)]
pub(crate) struct WgslStruct {
    pub(crate) attrs: WgslAttributes,
    pub(crate) vis: Visibility,
    pub(crate) ident: Ident,

    /// Rust fields in the struct including padding fields.
    ///
    /// Padding fields start with `PAD_PREFIX`, so that you can easily filter
    /// them out.
    pub(crate) fields: Vec<WgslField>,
    pub(crate) struct_layout: LayoutExt,
}

impl WgslStruct {
    pub(crate) fn new<F, G>(
        st: &ItemStruct,
        uniform: bool,
        mut find_layout: F,
        mut find_len: G,
    ) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
        G: FnMut(&Ident) -> Result<usize>,
    {
        if matches!(st.fields, Fields::Unnamed(_)) {
            return Err(Error::new(st.span(), "tuple struct is not allowd for now"));
        }
        if let Some(attr) = st.attrs.iter().find(|attr| attr.path().is_ident("repr")) {
            return Err(Error::new(attr.span(), "`repr` attribute is reserved"));
        }

        let mut offset = 0;
        let mut max_align = 1;
        let mut fields = Vec::new();

        let num_fields = st.fields.len();
        for (i, cur) in st.fields.iter().enumerate() {
            let (rust_layout, wgsl_layout, wgsl_field_align) =
                Self::field_to_layout(cur, uniform, &mut find_layout, &mut find_len)?;

            // Runtime sized array? then it must be the last member.
            if !rust_layout.is_sized && i < num_fields - 1 {
                return Err(Error::new(
                    cur.span(),
                    "runtime sized array must be the last member",
                ));
            }

            // Adds a preceding pad field if needed.
            let (pad, need) = Self::preceding_pad(offset, rust_layout, wgsl_layout);
            if need {
                let (f, layout) = Self::create_pad_field(fields.len(), pad);
                fields.push(WgslField {
                    attrs: WgslAttributes::new(),
                    vis: f.vis,
                    ident: f.ident.unwrap(),
                    ty: f.ty,
                    rust_layout: layout,
                    wgsl_layout: layout,
                    offset,
                });
            }
            offset += pad;

            // Adds the current field.
            let mut attrs = WgslAttributes::from_syn(cur.attrs.clone())?;
            if wgsl_field_align > 0 {
                let width = Index::from(wgsl_field_align);
                let attr: Attribute = parse_quote!(#[align(#width)]);
                attrs.push(WgslAttribute::from_syn(attr)?);
            }
            fields.push(WgslField {
                attrs,
                vis: cur.vis.clone(),
                ident: cur.ident.clone().unwrap(),
                ty: cur.ty.clone(),
                rust_layout,
                wgsl_layout,
                offset,
            });

            // Adds a following pad field if needed.
            let pad = Self::following_pad(rust_layout, wgsl_layout);
            if pad > 0 {
                let (f, layout) = Self::create_pad_field(fields.len(), pad);
                fields.push(WgslField {
                    attrs: WgslAttributes::new(),
                    vis: f.vis,
                    ident: f.ident.unwrap(),
                    ty: f.ty,
                    rust_layout: layout,
                    wgsl_layout: layout,
                    offset,
                });
            }

            // Adjusts the current offset and maximum alignment.
            offset += rust_layout.size;
            max_align = max_align.max(wgsl_layout.align);
        }

        // Creates layout for the struct.
        let size = round_up_by_align(offset, max_align);
        let is_sized = fields.iter().all(|f| f.rust_layout.is_sized);
        let struct_layout = LayoutExt::new(size, max_align, is_sized)
            .ok_or(Error::new(st.span(), "invalid layout"))?;

        // Records ident of the struct.
        insert_wgsl_path(st.ident.to_string());

        Ok(Self {
            attrs: FromSyn::from_syn(Self::filter_attributes(&st.attrs))?,
            vis: st.vis.clone(),
            ident: st.ident.clone(),
            fields,
            struct_layout,
        })
    }

    // Returns (Rust layout, WGSL layout, WGSL field align) for the given field.
    fn field_to_layout<F, G>(
        field: &Field,
        uniform: bool,
        find_layout: F,
        find_len: G,
    ) -> Result<(LayoutExt, LayoutExt, usize)>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
        G: FnMut(&Ident) -> Result<usize>,
    {
        const IN_ARRAY: bool = false;

        let (rust_layout, wgsl_layout) =
            Self::type_to_layout(&field.ty, IN_ARRAY, find_layout, find_len)?;
        debug_assert!(rust_layout.size <= wgsl_layout.size);
        debug_assert!(rust_layout.align <= wgsl_layout.align);

        // `align` attribute affects WGSL layout.
        let wgsl_layout = if let Some(value) = field.get_attribute_value("align") {
            let new_align = value
                .parse::<usize>()
                .map_err(|e| Error::new(field.span(), e))?;
            LayoutExt::new(wgsl_layout.size, new_align, wgsl_layout.is_sized)
                .ok_or(Error::new(field.span(), "invalid layout"))?
        } else {
            wgsl_layout
        };

        // `size` attribute affects WGSL layout.
        let wgsl_layout = if let Some(value) = field.get_attribute_value("size") {
            let new_size = value
                .parse::<usize>()
                .map_err(|e| Error::new(field.span(), e))?;
            LayoutExt::new(new_size, wgsl_layout.align, wgsl_layout.is_sized)
                .ok_or(Error::new(field.span(), "invalid layout"))?
        } else {
            wgsl_layout
        };

        // Uniform? then adjusts the WGSL layout. It may need extra attribute
        // in WGSL code.
        let (wgsl_layout, wgsl_field_align) = if uniform {
            match &field.ty {
                // [T; N] is not allowed in uniform when T's align is not 16*K.
                Type::Array(ty) => {
                    if wgsl_layout.align % 16 != 0 {
                        return Err(Error::new(
                            ty.elem.span(),
                            "alignment must be a multiple of 16 in uniform address space",
                        ));
                    }
                }
                // [T] is not allowed in uniform.
                Type::Slice(ty) => {
                    return Err(Error::new(
                        ty.span(),
                        "runtime sized array is not allowed in uniform address space",
                    ));
                }
                _ => {}
            }

            let new_layout = LayoutExt::new(
                wgsl_layout.size,
                round_up_by_align(wgsl_layout.align, 16),
                wgsl_layout.is_sized,
            )
            .unwrap();

            if wgsl_layout != new_layout {
                (new_layout, new_layout.align)
            } else {
                (new_layout, 0 /* No need to specify */)
            }
        } else {
            (wgsl_layout, 0 /* No need to specify */)
        };

        Ok((rust_layout, wgsl_layout, wgsl_field_align))
    }

    fn type_to_layout<F, G>(
        ty: &Type,
        in_arr: bool,
        mut find_layout: F,
        mut find_len: G,
    ) -> Result<(LayoutExt, LayoutExt)>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
        G: FnMut(&Ident) -> Result<usize>,
    {
        let (rust_layout, wgsl_layout) = match ty {
            // T: Returns (size: size of T, align of T)
            Type::Path(_) => {
                let ty_ident = last_type_path_ident(ty)?;
                let (rust_layout, wgsl_layout) = find_layout(ty_ident)?;

                if in_arr {
                    Self::is_ok_as_array_elem_type(ty)?;
                } else {
                    Self::is_ok_as_struct_member_type(ty)?;
                }
                if !rust_layout.is_sized {
                    return Err(Error::new(
                        ty.span(),
                        "type including runtime sized array cannot be a member of another struct",
                    ));
                }

                (rust_layout, wgsl_layout)
            }
            // [T; N]: Returns (size: N * stride of T, align: align of T)
            Type::Array(ty) => {
                const IN_ARRAY: bool = true;

                let len: usize = ty.len.evaluate(&mut find_len)?;
                let (elem_rust, elem_wgsl) =
                    Self::type_to_layout(&ty.elem, IN_ARRAY, find_layout, find_len)?;
                let rust_layout = elem_rust.to_array_layout(len);
                let wgsl_layout = elem_wgsl.to_array_layout(len);
                (rust_layout, wgsl_layout)
            }
            // [T]: Returns (size: stride of T, align: align of T)
            Type::Slice(ty) => {
                const IN_ARRAY: bool = true;
                const IS_SIZED: bool = false;

                let (elem_rust, elem_wgsl) =
                    Self::type_to_layout(&ty.elem, IN_ARRAY, find_layout, find_len)?;
                let rust_layout =
                    LayoutExt::new(elem_rust.stride(), elem_rust.align, IS_SIZED).unwrap();
                let wgsl_layout =
                    LayoutExt::new(elem_wgsl.stride(), elem_wgsl.align, IS_SIZED).unwrap();

                (rust_layout, wgsl_layout)
            }
            _ => return Err(Error::new(ty.span(), "cannot be compatible with WGSL")),
        };

        Ok((rust_layout, wgsl_layout))
    }

    /// Returns pair of required padding bytes and whether it is required
    /// explicitly.
    ///
    /// Preceding padding is required when Rust need an extra pad field due to
    /// alignment difference between Rust and WGSL. See an example below.
    ///
    /// ```text
    /// * Explicitly required padding
    /// ----[Rust]
    /// --------[WGSL]
    /// -------- : We need explicit padding as much as this amount.
    ///
    /// * Implicitly required padding
    /// ----[Rust]
    /// ----[WGSL]
    /// ---- : Rust will put this hidden pad automatically.
    /// ```
    const fn preceding_pad(
        offset: usize,
        rust_layout: LayoutExt,
        wgsl_layout: LayoutExt,
    ) -> (usize, bool) {
        debug_assert!(rust_layout.align <= wgsl_layout.align);

        let pad = round_up_by_align(offset, wgsl_layout.align) - offset;
        let need_explicit = rust_layout.align != wgsl_layout.align && pad > 0;

        (pad, need_explicit)
    }

    /// Returns pair of required following padding bytes.
    ///
    /// Following padding is required when Rust need an extra pad field due to
    /// size difference between Rust and WGSL. See an example below.
    ///
    /// ```text
    /// ----[Rust]
    /// ----[--WGSL--]
    ///           ---- : We need padding as much as this amount.
    /// ```
    const fn following_pad(rust_layout: LayoutExt, wgsl_layout: LayoutExt) -> usize {
        debug_assert!(rust_layout.size <= wgsl_layout.size);

        wgsl_layout.size - rust_layout.size
    }

    fn create_pad_field(i: usize, size: usize) -> (Field, LayoutExt) {
        const IS_SIZED: bool = true;

        let ident: TokenStream2 = format!("{PAD_PREFIX}{i}").parse().unwrap();
        let len = Index::from(size);
        let pad_field: Field = parse_quote! {
            #ident: std::mem::MaybeUninit<[u8; #len]>
        };
        let layout = LayoutExt::new(size, 1, IS_SIZED).unwrap();

        (pad_field, layout)
    }

    fn filter_attributes(attrs: &[Attribute]) -> Vec<Attribute> {
        let disallowed = [ATTR_UNIFORM];
        attrs
            .iter()
            .filter(|attr| !disallowed.iter().any(|s| attr.path().is_ident(&s)))
            .cloned()
            .collect()
    }

    pub(crate) fn layout(&self) -> LayoutExt {
        self.struct_layout
    }

    /// Returns true if the struct is sized.
    ///
    /// If the struct contains runtime sized array as the last member, it is not
    /// a sized struct, so that returns false in that case.
    fn is_sized(&self) -> bool {
        self.struct_layout.is_sized
    }

    #[rustfmt::skip]
    fn is_ok_as_array_elem_type(ty: &Type) -> Result<()> {
        let disallowed: [&str; 9] = [
            Vec2i::ident(), Vec3i::ident(), Vec4i::ident(),
            Vec2u::ident(), Vec3u::ident(), Vec4u::ident(),
            Vec2f::ident(), Vec3f::ident(), Vec4f::ident(),
        ];
        let alternatives: [&str; 9] = [
            WideVec2i::ident(), WideVec3i::ident(), WideVec4i::ident(),
            WideVec2u::ident(), WideVec3u::ident(), WideVec4u::ident(),
            WideVec2f::ident(), WideVec3f::ident(), WideVec4f::ident(),
        ];
        let ty_ident = last_type_path_ident(ty)?;
        let ident_str = &ty_ident.to_string();

        if let Some((i, _)) = disallowed.iter().enumerate().find(|(_, &v)| v == ident_str) {
            Err(Error::new(
                ty.span(),
                format!(
                    "`{ty_ident}` is not allowed in array due to alignment. please use `{}` instead",
                    alternatives[i]
                ),
            ))
        } else {
            Ok(())
        }
    }

    #[rustfmt::skip]
    fn is_ok_as_struct_member_type(ty: &Type) -> Result<()> {
        let disallowed: [&str; 9] = [
            WideVec2i::ident(), WideVec3i::ident(), WideVec4i::ident(),
            WideVec2u::ident(), WideVec3u::ident(), WideVec4u::ident(),
            WideVec2f::ident(), WideVec3f::ident(), WideVec4f::ident(),
        ];
        let alternatives: [&str; 9] = [
            Vec2i::ident(), Vec3i::ident(), Vec4i::ident(),
            Vec2u::ident(), Vec3u::ident(), Vec4u::ident(),
            Vec2f::ident(), Vec3f::ident(), Vec4f::ident(),
        ];
        let ty_ident = last_type_path_ident(ty)?;
        let ident_str = &ty_ident.to_string();

        if let Some((i, _)) = disallowed.iter().enumerate().find(|(_, &v)| v == ident_str) {
            Err(Error::new(
                ty.span(),
                format!(
                    "`{ty_ident}` is not allowed in struct due to size. please use `{}` instead",
                    alternatives[i]
                ),
            ))
        } else {
            Ok(())
        }
    }

    fn to_unsized_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            attrs,
            vis,
            ident,
            fields,
            ..
        } = self;

        let inner_ident = format_ident!("__{}", ident);
        let fields = fields.iter().filter(|f| !f.is_pad());
        let decl_new = Self::decl_unsized_outer_new(&inner_ident, fields.clone());
        let decl_inner = Self::decl_unsized_inner(vis, &inner_ident, fields.clone());

        tokens.append_all(quote! {
            #attrs
            #[repr(transparent)]
            #vis struct #ident(#inner_ident);

            impl #ident {
                #vis #decl_new
            }

            impl std::ops::Deref for #ident {
                type Target = #inner_ident;
                fn deref(&self) -> &Self::Target { &self.0 }
            }

            impl std::ops::DerefMut for #ident {
                fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
            }

            #decl_inner
        });
    }

    fn decl_unsized_outer_new<'a, I>(inner_ident: &Ident, fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let params = fields.clone().take(cnt).map(|f| {
            let (ident, ty) = (&f.ident, &f.ty);
            quote! { #ident: #ty }
        });

        let writes = fields.take(cnt).map(|f| {
            let ident = &f.ident;
            let offset = format_ident!("OFFSET_{}", ident.to_uppercase());
            quote! {
                inner.__write(#inner_ident::#offset, #ident);
            }
        });

        quote! {
            fn new( #(#params),* ) -> Self {
                let mut inner = #inner_ident(vec![0; #inner_ident::OFFSET_LAST]);
                #(#writes)*
                Self(inner)
            }
        }
    }

    fn decl_unsized_inner<'a, I>(vis: &Visibility, ident: &Ident, fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let decl_const = Self::decl_unsized_inner_const(fields.clone());
        let decl_drop = Self::decl_unsized_inner_drop(ident, fields.clone());

        let last_field = Self::last_field(fields.clone());
        let last_elem_type = elem_type(&last_field.ty).unwrap();

        let get_set = Self::decl_unsized_inner_get_set(vis, fields.clone());

        quote! {
            #[repr(transparent)]
            #vis struct #ident(Vec<u8>);

            impl #ident {
                #decl_const

                #get_set

                #vis fn truncate(&mut self, len: usize) {
                    let new_len = Self::OFFSET_LAST + Self::STRIDE_LAST * len;
                    let cur_len = self.0.len();
                    if new_len >= cur_len {
                        return;
                    }
                    unsafe {
                        let e: *const #last_elem_type =
                            self.0.as_ptr().add(new_len).cast();
                        let mut p: *const #last_elem_type =
                            self.0.as_ptr().add(cur_len - Self::STRIDE_LAST).cast();
                        while p >= e {
                            drop(std::ptr::read(p));
                            p = p.sub(1);
                        }
                        self.0.set_len(new_len);
                    }
                }

                #vis fn extend_with<F>(&mut self, len: usize, mut f: F)
                where
                    F: FnMut(usize) -> #last_elem_type
                {
                    let new_len = Self::OFFSET_LAST + Self::STRIDE_LAST * len;
                    let cur_len = self.0.len();
                    if new_len <= cur_len {
                        return;
                    }
                    unsafe {
                        self.0.reserve_exact(new_len - cur_len);
                        let e: *mut #last_elem_type =
                            self.0.as_mut_ptr().add(new_len).cast();
                        let mut p: *mut #last_elem_type =
                            self.0.as_mut_ptr().add(cur_len).cast();
                        let mut i = (cur_len - Self::OFFSET_LAST) / Self::STRIDE_LAST;
                        while p < e {
                            std::ptr::write(p, f(i));
                            p = p.add(1);
                            i += 1;
                        }
                        self.0.set_len(new_len);
                    }
                }

                #vis fn shrink_to_fit(&mut self) {
                    self.0.shrink_to_fit();
                }

                #vis fn capacity_bytes(&self) -> usize {
                    self.0.capacity()
                }

                fn __as_bytes(&self) -> &[u8] {
                    self.0.as_slice()
                }

                fn __as_mut_bytes(&mut self) -> &mut [u8] {
                    self.0.as_mut_slice()
                }

                fn __write<T>(&mut self, offset: usize, value: T) {
                    unsafe {
                        let ptr: *mut T = self.0.as_mut_ptr().add(offset).cast();
                        std::ptr::write(ptr, value);
                    }
                }

                fn __get<T>(&self, offset: usize) -> &T {
                    unsafe {
                        let ptr: *const T = self.0.as_ptr().add(offset).cast();
                        ptr.as_ref().unwrap_unchecked()
                    }
                }

                fn __set<T>(&mut self, offset: usize, value: T) -> T {
                    unsafe {
                        let ptr: *const T = self.0.as_ptr().add(offset).cast();
                        let old = std::ptr::read(ptr);
                        std::ptr::write(ptr.cast_mut(), value);
                        old
                    }
                }

                fn __read<T>(&self, offset: usize) -> T {
                    unsafe {
                        let ptr: *const T = self.0.as_ptr().add(offset).cast();
                        std::ptr::read(ptr)
                    }
                }
            }

            #decl_drop
        }
    }

    fn decl_unsized_inner_const<'a, I>(fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let decl_const_offsets = fields.clone().take(cnt).map(|f| {
            let const_name = format_ident!("OFFSET_{}", f.ident.to_uppercase());
            let offset = Index::from(f.offset);
            quote! {
                const #const_name: usize = #offset;
            }
        });

        let last_field = Self::last_field(fields);
        let offset = Index::from(last_field.offset);
        let wgsl_stride = Index::from(Self::elem_wgsl_stride(last_field));
        let decl_const_last = quote! {
            const OFFSET_LAST: usize = #offset;
            const STRIDE_LAST: usize = #wgsl_stride;
        };

        quote! {
            #(#decl_const_offsets)*
            #decl_const_last
        }
    }

    fn decl_unsized_inner_get_set<'a, I>(vis: &Visibility, fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let get_set_methods_except_last = fields.clone().take(cnt).map(|f| {
            let fn_get_name = format_ident!("get_{}", f.ident);
            let fn_set_name = format_ident!("set_{}", f.ident);
            let offset = format_ident!("OFFSET_{}", f.ident.to_uppercase());
            let ty = &f.ty;

            quote! {
                #vis fn #fn_get_name(&self) -> &#ty {
                    self.__get(Self::#offset)
                }

                #vis fn #fn_set_name(&mut self, value: #ty) -> #ty {
                    self.__set(Self::#offset, value)
                }
            }
        });

        let last_field = Self::last_field(fields);
        let last_elem_type = elem_type(&last_field.ty).unwrap();
        let fn_get_name = format_ident!("get_{}", last_field.ident);
        let fn_set_name = format_ident!("get_mut_{}", last_field.ident);
        let get_set_last = quote! {
            #vis fn #fn_get_name(&self) -> &[#last_elem_type] {
                unsafe {
                    let len = (self.0.len() - Self::OFFSET_LAST) / Self::STRIDE_LAST;
                    if len > 0 {
                        let ptr: *const #last_elem_type = self.0.as_ptr().add(Self::OFFSET_LAST).cast();
                        std::slice::from_raw_parts(ptr, len)
                    } else {
                        &[] // Due to pointer alignment
                    }
                }
            }

            #vis fn #fn_set_name(&mut self) -> &mut [#last_elem_type] {
                unsafe {
                    let len = (self.0.len() - Self::OFFSET_LAST) / Self::STRIDE_LAST;
                    if len > 0 {
                        let ptr: *mut #last_elem_type = self.0.as_mut_ptr().add(Self::OFFSET_LAST).cast();
                        std::slice::from_raw_parts_mut(ptr, len)
                    } else {
                        &mut [] // Due to pointer alignment
                    }
                }
            }
        };

        quote! {
            #(#get_set_methods_except_last)*
            #get_set_last
        }
    }

    fn decl_unsized_inner_drop<'a, I>(ident: &Ident, fields: I) -> TokenStream2
    where
        I: Iterator<Item = &'a WgslField> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let drop_fields = fields.take(cnt).map(|f| {
            let const_name = format_ident!("OFFSET_{}", f.ident.to_uppercase());
            let ty = &f.ty;
            quote! {
                std::ptr::drop_in_place(
                    self.0.as_mut_ptr().add(Self::#const_name).cast::<#ty>()
                );
            }
        });

        quote! {
            impl Drop for #ident {
                fn drop(&mut self) {
                    unsafe { #(#drop_fields)* }
                    self.truncate(0);
                }
            }
        }
    }

    fn last_field<'a, I>(fields: I) -> &'a WgslField
    where
        I: Iterator<Item = &'a WgslField>,
    {
        fields
            .last()
            .expect("internal error: the last member must be runtime sized array")
    }

    fn elem_wgsl_stride(field: &WgslField) -> usize {
        field.wgsl_layout.stride()
    }

    fn decl_sized_new(&self) -> TokenStream2 {
        let Self {
            vis, ident, fields, ..
        } = self;

        let params = fields.iter().filter(|f| !f.is_pad()).map(|f| {
            let (ident, ty) = (&f.ident, &f.ty);
            quote! { #ident: #ty }
        });

        let assign_fields = fields.iter().map(|f| {
            let ident = &f.ident;
            if !f.is_pad() {
                quote! { #ident: #ident }
            } else {
                quote! { #ident: std::mem::MaybeUninit::uninit() }
            }
        });

        quote! {
            impl #ident {
                #vis const fn new(#(#params),*) -> Self {
                    Self {
                        #(#assign_fields),*
                    }
                }
            }
        }
    }
}

impl ToTokens for WgslStruct {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            attrs,
            vis,
            ident,
            fields,
            struct_layout,
        } = self;

        let decl_new = self.decl_sized_new();

        if !struct_layout.is_sized {
            // Contaiing runtime sized array.
            self.to_unsized_tokens(tokens);
        } else if self.layout().size > 0 {
            // Not a ZST.
            let struct_align = Index::from(struct_layout.align);
            tokens.append_all(quote! {
                #attrs
                #[repr(C, align(#struct_align))]
                #vis struct #ident {
                    #(#fields),*
                }

                #decl_new
            });
        } else {
            // Allow ZST?
            tokens.append_all(quote! {
                #attrs
                #vis struct #ident;
            });
        }

        tokens.append_all(self.runtime_tokens());
    }
}

impl ToWgslString for WgslStruct {
    fn write_wgsl_string(&self, buf: &mut String) {
        buf.push_str("struct ");
        buf.push_str(&self.ident.to_string());
        buf.push('{');
        for f in self.fields.iter().filter(|f| !f.is_pad()) {
            f.write_wgsl_string(buf);
            buf.push(',');
        }
        if let Some(c) = buf.pop() {
            if c != ',' {
                buf.push(c);
            }
        }
        buf.push('}');
    }
}

impl RuntimeWgslToken for WgslStruct {
    fn runtime_tokens(&self) -> TokenStream2 {
        let ident = &self.ident;

        let fields = self.fields.iter().filter_map(|f| {
            if !f.is_pad() {
                let struct_member = f.runtime_tokens();
                Some(quote! {
                    wgsl_struct.members.push(#struct_member);
                })
            } else {
                None
            }
        });

        let as_bytes = if self.is_sized() {
            quote! {
                fn as_bytes(&self) -> &[u8] {
                    let ptr = (self as *const Self).cast::<u8>();
                    let len = size_of::<Self>();
                    unsafe { std::slice::from_raw_parts(ptr, len) }
                }
                fn as_mut_bytes(&mut self) -> &mut [u8] {
                    let ptr = (self as *mut Self).cast::<u8>();
                    let len = size_of::<Self>();
                    unsafe { std::slice::from_raw_parts_mut(ptr, len) }
                }
            }
        } else {
            quote! {
                fn as_bytes(&self) -> &[u8] {
                    self.__as_bytes()
                }
                fn as_mut_bytes(&mut self) -> &mut [u8] {
                    self.__as_mut_bytes()
                }
            }
        };

        let mut wgsl_code = String::new();
        self.write_wgsl_string(&mut wgsl_code);

        quote! {
            impl my_wgsl::BeWgslStruct for #ident {
                fn be_struct() -> my_wgsl::WgslStruct {
                    let mut wgsl_struct = my_wgsl::WgslStruct {
                        ident: stringify!(#ident).to_owned(),
                        members: Vec::new()
                    };
                    #(#fields)*
                    wgsl_struct
                }

                #as_bytes
            }

            impl #ident {
                pub const WGSL: &str = #wgsl_code;
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct WgslField {
    pub(crate) attrs: WgslAttributes,

    pub(crate) vis: Visibility,

    pub(crate) ident: Ident,

    pub(crate) ty: Type,

    /// Rust layout.
    pub(crate) rust_layout: LayoutExt,

    /// WGSL layout.
    ///
    /// This could me meaningless.
    pub(crate) wgsl_layout: LayoutExt,

    /// Rust & WGSL offset.
    ///
    /// Both offsets are kept to be the same by the crate.
    pub(crate) offset: usize,
}

impl WgslField {
    fn is_pad(&self) -> bool {
        self.ident.to_string().starts_with(PAD_PREFIX)
    }
}

impl ToTokens for WgslField {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.attrs.to_tokens(tokens);
        self.vis.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        tokens.append(Punct::new(':', Spacing::Alone));
        self.ty.to_tokens(tokens);
    }
}

impl ToWgslString for WgslField {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        buf.push_str(&self.ident.to_string());
        buf.push(':');
        buf.push_str(&self.ty.wgsl_string());
    }
}

impl RuntimeWgslToken for WgslField {
    fn runtime_tokens(&self) -> TokenStream2 {
        let ident = &self.ident;
        let ty = self.ty.wgsl_string();

        let push_attrs = self.attrs.iter().map(|a| {
            let attr = a.runtime_tokens();
            quote! {
                struct_member.attrs.push(#attr);
            }
        });

        quote! {
            {
                let mut struct_member = my_wgsl::StructMember::new(
                    stringify!(#ident).to_owned(),
                    #ty.to_owned()
                );
                #(#push_attrs)*
                struct_member
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LayoutExt {
    pub(crate) size: usize,
    pub(crate) align: usize,
    pub(crate) is_sized: bool,
}

impl LayoutExt {
    pub(crate) const fn new(size: usize, align: usize, is_sized: bool) -> Option<Self> {
        if (align == 0)
            || (!align.is_power_of_two())
            || (round_up_by_align(size, align) > isize::MAX as usize)
        {
            None
        } else {
            Some(Self {
                size,
                align,
                is_sized,
            })
        }
    }

    pub(crate) const fn to_array_layout(self, len: usize) -> Self {
        debug_assert!(self.is_sized);

        unsafe { Self::new(self.size * len, self.align, self.is_sized).unwrap_unchecked() }
    }

    pub(crate) const fn stride(&self) -> usize {
        round_up_by_align(self.size, self.align)
    }
}
