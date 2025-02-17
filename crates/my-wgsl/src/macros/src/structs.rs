use super::{
    util::*, Vec2f, Vec2i, Vec2u, Vec3f, Vec3i, Vec3u, Vec4f, Vec4i, Vec4u, WideVec2f, WideVec2i,
    WideVec2u, WideVec3f, WideVec3i, WideVec3u, WideVec4f, WideVec4i, WideVec4u,
};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::alloc::{Layout, LayoutError};
use syn::{
    parse_quote, spanned::Spanned, Attribute, Error, Field, Fields, Ident, Index, ItemStruct,
    Result, Type, Visibility,
};

/// Attribute to be compatible with uniform address space.
pub(crate) const ATTR_UNIFORM: &str = "uniform";

/// Ident prefix for Rust padding fields.
pub(crate) const PAD_PREFIX: &str = "__pad";

#[derive(Debug)]
pub(crate) struct WgslStruct {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    pub(crate) ident: Ident,

    /// Rust fields in the struct including padding fields.
    ///
    /// Padding fields start with `PAD_PREFIX`, so that you can easily filter
    /// them out.
    pub(crate) fields: Vec<FieldExt>,
    pub(crate) struct_layout: LayoutExt,
}

impl WgslStruct {
    pub(crate) fn new<F>(st: &ItemStruct, uniform: bool, mut search: F) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
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
                Self::field_to_layout(cur, uniform, &mut search)?;

            // Runtime sized array? then it must be the last member.
            if !rust_layout.is_sized() && i < num_fields - 1 {
                return Err(Error::new(
                    cur.span(),
                    "runtime sized array must be the last member",
                ));
            }

            // Adds a pad field if needed.
            let (pad, need) = Self::required_pad(offset, rust_layout, wgsl_layout);
            if need {
                let (field, field_layout) = Self::create_pad_field(fields.len(), pad);
                fields.push(FieldExt::new(field, field_layout, field_layout, offset, 0));
            }
            offset += pad;

            // Adds the current field.
            fields.push(FieldExt::new(
                cur.clone(),
                rust_layout,
                wgsl_layout,
                offset,
                wgsl_field_align,
            ));

            // Adjusts the current offset and maximum alignment.
            offset += rust_layout.size();
            max_align = max_align.max(wgsl_layout.align());
        }

        // Creates layout for the struct.
        let size = round_up_by_align(offset, max_align);
        let is_sized = fields.iter().all(|f| f.rust_layout.sized);
        let struct_layout = LayoutExt::new(
            Layout::from_size_align(size, max_align).map_err(|e| Error::new(st.span(), e))?,
            is_sized,
        );

        Ok(Self {
            attrs: Self::filter_attributes(&st.attrs),
            vis: st.vis.clone(),
            ident: st.ident.clone(),
            fields,
            struct_layout,
        })
    }

    // Returns (Rust layout, WGSL layout, WGSL field align) for the given field.
    fn field_to_layout<F>(
        field: &Field,
        uniform: bool,
        search: F,
    ) -> Result<(LayoutExt, LayoutExt, usize)>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
    {
        const IN_ARRAY: bool = false;

        let (rust_layout, wgsl_layout) = Self::type_to_layout(&field.ty, search, IN_ARRAY)?;
        debug_assert!(rust_layout.size() <= wgsl_layout.size());
        debug_assert!(rust_layout.align() <= wgsl_layout.align());

        // Uniform? then adjusts the WGSL layout. It may need extra attribute
        // in WGSL code.
        let (wgsl_layout, wgsl_field_align) = if uniform {
            match &field.ty {
                // [T; N] is not allowed in uniform when T's align is not 16*K.
                Type::Array(ty) => {
                    if wgsl_layout.align() % 16 != 0 {
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

            let new_layout = unsafe { wgsl_layout.align_to(16).unwrap_unchecked() };
            if wgsl_layout != new_layout {
                (new_layout, new_layout.align())
            } else {
                (new_layout, 0 /* No need to specify */)
            }
        } else {
            (wgsl_layout, 0 /* No need to specify */)
        };

        Ok((rust_layout, wgsl_layout, wgsl_field_align))
    }

    fn type_to_layout<F>(ty: &Type, mut search: F, in_arr: bool) -> Result<(LayoutExt, LayoutExt)>
    where
        F: FnMut(&Ident) -> Result<(LayoutExt, LayoutExt)>,
    {
        let (rust_layout, wgsl_layout) = match ty {
            // T: Returns (size: size of T, align of T)
            Type::Path(ty) => {
                let ty_ident = ty
                    .path
                    .segments
                    .last()
                    .map(|seg| &seg.ident)
                    .ok_or(Error::new(ty.span(), "invalid type path"))?;
                let (rust_layout, wgsl_layout) = search(ty_ident)?;

                Self::check_type_availability(
                    ty.span(),
                    &ty_ident.to_string(),
                    in_arr,
                    rust_layout,
                )?;

                (rust_layout, wgsl_layout)
            }
            // [T; N]: Returns (size: stride of T, align: align of T)
            Type::Array(ty) => {
                const IN_ARRAY: bool = true;

                let (elem_rust, elem_wgsl) = Self::type_to_layout(&ty.elem, search, IN_ARRAY)?;
                let len = expr_to_integer(&ty.len)
                    .ok_or(Error::new(ty.span(), "expected literal"))?
                    as usize;
                let rust_layout = Self::array_layout(elem_rust, len, ty.span())?;
                let wgsl_layout = Self::array_layout(elem_wgsl, len, ty.span())?;
                (rust_layout, wgsl_layout)
            }
            // [T]: Returns (size: stride of T, align: align of T)
            Type::Slice(ty) => {
                const IN_ARRAY: bool = true;
                const IS_SIZED: bool = false;

                let (elem_rust, elem_wgsl) = Self::type_to_layout(&ty.elem, search, IN_ARRAY)?;

                let rust_layout = LayoutExt::new(
                    Layout::from_size_align(elem_rust.stride(), elem_rust.align()).unwrap(),
                    IS_SIZED,
                );
                let wgsl_layout = LayoutExt::new(
                    Layout::from_size_align(elem_wgsl.stride(), elem_wgsl.align()).unwrap(),
                    IS_SIZED,
                );

                (rust_layout, wgsl_layout)
            }
            _ => return Err(Error::new(ty.span(), "cannot be compatible with WGSL")),
        };

        Ok((rust_layout, wgsl_layout))
    }

    fn array_layout(elem_layout: LayoutExt, len: usize, span: Span) -> Result<LayoutExt> {
        const IS_SIZED: bool = true;

        Ok(LayoutExt::new(
            Layout::from_size_align(elem_layout.stride() * len, elem_layout.align())
                .map_err(|e| Error::new(span, e))?,
            IS_SIZED,
        ))
    }

    /// Returns pair of padding bytes and whether it is required explicitly.
    ///
    /// If true, we need to add a pad field, otherwise Rust will add a WGSL
    /// compatible padding silently.
    const fn required_pad(
        offset: usize,
        rust_layout: LayoutExt,
        wgsl_layout: LayoutExt,
    ) -> (usize, bool) {
        debug_assert!(rust_layout.align() <= wgsl_layout.align());

        let pad = round_up_by_align(offset, wgsl_layout.align()) - offset;
        let need_explicit = rust_layout.align() != wgsl_layout.align() && pad > 0;

        (pad, need_explicit)
    }

    fn create_pad_field(i: usize, size: usize) -> (Field, LayoutExt) {
        const IS_SIZED: bool = true;

        let ident: TokenStream2 = format!("{PAD_PREFIX}{i}").parse().unwrap();
        let len = Index::from(size);
        let pad_field: Field = parse_quote! {
            #ident: std::mem::MaybeUninit<[u8; #len]>
        };
        let layout = LayoutExt::new(Layout::from_size_align(size, 1).unwrap(), IS_SIZED);
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
        self.struct_layout.is_sized()
    }

    fn check_type_availability(
        span: Span,
        ident: &str,
        in_arr: bool,
        layout: LayoutExt,
    ) -> Result<()> {
        if in_arr {
            const DISALLOWED: [&str; 9] = [
                Vec2i::ident(),
                Vec3i::ident(),
                Vec4i::ident(),
                Vec2u::ident(),
                Vec3u::ident(),
                Vec4u::ident(),
                Vec2f::ident(),
                Vec3f::ident(),
                Vec4f::ident(),
            ];
            const ALTERNATIVES: [&str; 9] = [
                WideVec2i::ident(),
                WideVec3i::ident(),
                WideVec4i::ident(),
                WideVec2u::ident(),
                WideVec3u::ident(),
                WideVec4u::ident(),
                WideVec2f::ident(),
                WideVec3f::ident(),
                WideVec4f::ident(),
            ];
            if let Some((i, _)) = DISALLOWED.iter().enumerate().find(|(_, &v)| v == ident) {
                return Err(Error::new(
                    span,
                    format!("`{ident}` is not allowed in array due to alignment. please use `{}` instead", ALTERNATIVES[i])
                ));
            }
        }

        if !layout.is_sized() {
            return Err(Error::new(
                span,
                "type including runtime sized array cannot be a member of another struct",
            ));
        }

        Ok(())
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
            #(#attrs)*
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
        I: Iterator<Item = &'a FieldExt> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let params = fields.clone().take(cnt).map(|f| {
            let ident = f.get_ident();
            let ty = f.get_type();
            quote! { #ident: #ty }
        });

        let writes = fields.take(cnt).map(|f| {
            let ident = f.get_ident();
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
        I: Iterator<Item = &'a FieldExt> + Clone,
    {
        let decl_const = Self::decl_unsized_inner_const(fields.clone());
        let decl_drop = Self::decl_unsized_inner_drop(ident, fields.clone());

        let last_field = Self::last_field(fields.clone());
        let last_elem_type = elem_type(last_field.get_type()).unwrap();

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
        I: Iterator<Item = &'a FieldExt> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let decl_const_offsets = fields.clone().take(cnt).map(|f| {
            let const_name = format_ident!("OFFSET_{}", f.get_ident().to_uppercase());
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
        I: Iterator<Item = &'a FieldExt> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let get_set_methods_except_last = fields.clone().take(cnt).map(|f| {
            let fn_get_name = format_ident!("get_{}", f.get_ident());
            let fn_set_name = format_ident!("set_{}", f.get_ident());
            let offset = format_ident!("OFFSET_{}", f.get_ident().to_uppercase());
            let ty = f.get_type();

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
        let last_elem_type = elem_type(last_field.get_type()).unwrap();
        let fn_get_name = format_ident!("get_{}", last_field.get_ident());
        let fn_set_name = format_ident!("get_mut_{}", last_field.get_ident());
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
        I: Iterator<Item = &'a FieldExt> + Clone,
    {
        let cnt = fields.clone().count() - 1; // Except the last member

        let drop_fields = fields.take(cnt).map(|f| {
            let const_name = format_ident!("OFFSET_{}", f.get_ident().to_uppercase());
            let ty = f.get_type();
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

    fn last_field<'a, I>(fields: I) -> &'a FieldExt
    where
        I: Iterator<Item = &'a FieldExt>,
    {
        fields
            .last()
            .expect("internal error: the last member must be runtime sized array")
    }

    fn elem_wgsl_stride(field: &FieldExt) -> usize {
        field.wgsl_layout.stride()
    }

    fn to_wgsl_code_tokens(&self, tokens: &mut TokenStream2) {
        let Self { ident, fields, .. } = self;

        let fields = fields.iter().filter(|f| !f.is_pad());
        let insert_field_aligns = fields.clone().map(|f| {
            if f.wgsl_field_align > 0 {
                let align = Index::from(f.wgsl_field_align);
                quote! {
                    member.insert_attribute("align", Some(stringify!(#align)));
                }
            } else {
                quote! {}
            }
        });
        let field_idents = fields.clone().map(|f| f.get_ident());
        let field_types = fields.map(|f| f.get_type());

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

        tokens.append_all(quote! {
            impl my_wgsl::Identify for #ident {
                fn ident() -> String { stringify!(#ident).to_owned() }
            }
            impl my_wgsl::BeWgslStruct for #ident {
                fn be_struct() -> my_wgsl::WgslStruct {
                    let mut st = my_wgsl::WgslStruct {
                        ident: <Self as my_wgsl::Identify>::ident(),
                        members: Vec::new()
                    };

                    #(
                        let mut member = my_wgsl::StructMember::new(
                            stringify!(#field_idents).to_owned(),
                            <#field_types as my_wgsl::Identify>::ident()
                        );
                        #insert_field_aligns
                        st.members.push(member);
                    )*

                    st
                }

                #as_bytes
            }
        });
    }

    fn decl_sized_new(&self) -> TokenStream2 {
        let Self {
            vis, ident, fields, ..
        } = self;

        let params = fields.iter().filter(|f| !f.is_pad()).map(|f| {
            let ident = f.get_ident();
            let ty = f.get_type();
            quote! { #ident: #ty }
        });

        let assign_fields = fields.iter().map(|f| {
            let ident = f.get_ident();
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

        if !struct_layout.is_sized() {
            // Contaiing runtime sized array.
            self.to_unsized_tokens(tokens);
        } else if self.layout().size() > 0 {
            // Not a ZST.
            let struct_align = Index::from(struct_layout.align());
            tokens.append_all(quote! {
                #(#attrs)*
                #[repr(C, align(#struct_align))]
                #vis struct #ident {
                    #(#fields),*
                }

                #decl_new
            });
        } else {
            // Allow ZST?
            tokens.append_all(quote! {
                #(#attrs)*
                #vis struct #ident;
            });
        }

        self.to_wgsl_code_tokens(tokens);
    }
}

#[derive(Debug)]
pub(crate) struct FieldExt {
    field: Field,

    /// Rust layout.
    rust_layout: LayoutExt,

    /// WGSL layout.
    ///
    /// This could me meaningless.
    wgsl_layout: LayoutExt,

    /// Rust & WGSL offset.
    ///
    /// Both offsets are kept to be the same by the crate.
    offset: usize,

    /// Needs for `@align(K)` in WGSL code generation.
    ///
    /// * 0: No need
    /// * K: Need to attach `@align(K)` to the field.
    wgsl_field_align: usize,
}

impl FieldExt {
    const fn new(
        field: Field,
        rust_layout: LayoutExt,
        wgsl_layout: LayoutExt,
        offset: usize,
        wgsl_field_align: usize,
    ) -> Self {
        Self {
            field,
            rust_layout,
            wgsl_layout,
            offset,
            wgsl_field_align,
        }
    }

    fn get_ident(&self) -> &Ident {
        self.field.ident.as_ref().unwrap()
    }

    fn get_type(&self) -> &Type {
        &self.field.ty
    }

    fn is_pad(&self) -> bool {
        self.get_ident().to_string().starts_with(PAD_PREFIX)
    }
}

impl ToTokens for FieldExt {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.field.to_tokens(tokens);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LayoutExt {
    layout: Layout,
    sized: bool,
}

impl LayoutExt {
    pub(crate) const fn new(layout: Layout, sized: bool) -> Self {
        Self { layout, sized }
    }

    const fn size(&self) -> usize {
        self.layout.size()
    }

    const fn align(&self) -> usize {
        self.layout.align()
    }

    const fn stride(&self) -> usize {
        round_up_by_align(self.size(), self.align())
    }

    const fn is_sized(&self) -> bool {
        self.sized
    }

    fn align_to(&self, align: usize) -> std::result::Result<Self, LayoutError> {
        self.layout
            .align_to(align)
            .map(|l| Self::new(l, self.sized))
    }
}
