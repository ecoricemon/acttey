use proc_macro::TokenStream;
use proc_macro2::{
    Delimiter as Delimiter2, Span, TokenStream as TokenStream2, TokenTree as TokenTree2,
};
use quote::quote;
use std::{borrow::Cow, num::NonZeroU32};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::{Brace, Paren},
    Attribute, Expr, ExprLit, Field, Fields, FieldsNamed, FnArg, GenericArgument, Ident, Index,
    ItemStruct, Lit, LitInt, LitStr, Meta, MetaList, Pat, Path, PathArguments, PathSegment, Token,
    Type, TypeArray,
};

// TODO: if the structure contain runtime sized array, we can't convert the structure into &[u8].
#[proc_macro_attribute]
pub fn wgsl_struct(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parses token streams.
    let attr = parse_macro_input!(attr as StructAttribute);
    let addr_space = AddressSpace::from(&attr);
    let mut item_struct = parse_macro_input!(item as ItemStruct);
    let st_ident = &item_struct.ident;

    // We're going to put some pad fields in the structure silently.
    // In order for hiding those pad fields from users, we need named field.
    match &item_struct.fields {
        Fields::Named(_) => {}
        _ => panic!("tuple or unit struct are not allowed"),
    }

    // Each field in the structure must have its own size and alignment.
    // so blocks both #[repr(align(n))] and #[repr(packed)] which could conflict.
    let repr = Repr::from(&item_struct.attrs);
    assert!(repr.align.is_none(), "#[repr(align(n))] is not allowed");
    assert!(repr.packed.is_none(), "#[repr(packed)] is not allowed");

    // Let's put some pad fields in the structure.
    assert!(
        !item_struct.fields.is_empty(),
        "empty struct is not allowed"
    );
    let last = item_struct.fields.len() - 1;
    let mut pos = 0;
    let mut padded_fields = Vec::new();
    let mut finfos = Vec::new();
    let mut offsets = Vec::new();
    for (i, mut field) in item_struct.fields.iter().cloned().enumerate() {
        // Creates field info and removes attributes immediately.
        let finfo = FieldInfo::from(&field);
        field.attrs.clear();

        // Validates the field.
        validate_field(&finfo, i == last, addr_space == AddressSpace::Uniform);

        // Hidden field acts like a pad in WGSL.
        if finfo.is_wgsl_hidden() {
            padded_fields.push(field);
            continue;
        }

        // Calculates required padding size.
        let offset = round_up_by_align(finfo.align.get(), pos);
        let pad_size = offset - pos;

        // Moves pos on to the next.
        let size = finfo.size.map(|v| v.get()).unwrap_or(0);
        pos = offset + size;

        // Inserts a pad field if it's needed.
        if pad_size > 0 {
            push_pad_field(&mut padded_fields, pad_size);
        }

        // Pushes.
        offsets.push(offset); // For validation.
        finfos.push(finfo);
        padded_fields.push(field);
    }

    // Calculates structure align.
    // ref: https://www.w3.org/TR/WGSL/#address-space-layout-constraints
    let mut st_align = finfos.iter().map(|finfo| finfo.align.get()).max().unwrap();
    if addr_space == AddressSpace::Uniform {
        st_align = round_up_by_align(16, st_align);
    }

    // Puts in the last explicit pad.
    let st_size = round_up_by_align(st_align, pos);
    let pad_size = st_size - pos;
    if pad_size > 0 {
        push_pad_field(&mut padded_fields, pad_size);
    }

    // Replaces fields to padded fields in the structure.
    let new_fields: FieldsNamed = parse_quote! { { #(#padded_fields),* } };
    item_struct.fields = Fields::Named(new_fields);

    // Adds #[repr(C)] if the structure doesn't have it.
    // It helps us put in padding fields easily.
    if repr.c.is_none() {
        let attr: Attribute = parse_quote! { #[repr(C)] };
        item_struct.attrs.push(attr);
    }

    // Adds #[repr(align(n))].
    let align = Index::from(st_align as usize);
    let attr: Attribute = parse_quote! { #[repr(align(#align))] };
    item_struct.attrs.push(attr);

    // Implements `my_wgsl::AsStructure`
    let impl_as_structure = impl_as_structure(st_ident, &finfos);

    // Implements `From` for &[u8] if the structure doesn't contain runtime sized field.
    let is_last_zero_sized = finfos
        .iter()
        .last()
        .map(|finfo| finfo.size.is_none())
        .unwrap();
    let impl_from_for_u8_slice = if !is_last_zero_sized {
        impl_from_for_u8_slice(st_ident, st_size as usize)
    } else {
        TokenStream2::new()
    };

    // Validates the structure.
    let validate = validate_wgsl_struct(&finfos, st_ident, st_size, st_align, &offsets);

    TokenStream::from(quote! {
        #item_struct
        #impl_as_structure
        #impl_from_for_u8_slice
        #validate
    })
}

fn push_pad_field(fields: &mut Vec<Field>, pad_size: u32) {
    let pad_ident: TokenStream2 = format!("_pad{}", fields.len()).parse().unwrap();
    let pad_size = Index::from(pad_size as usize);
    let pad_field: Field = parse_quote! { #pad_ident: [u8; #pad_size] };
    fields.push(pad_field);
}

fn impl_as_structure(st_ident: &Ident, finfos: &[FieldInfo]) -> TokenStream2 {
    // For example, #[ignore] field is supposed to be hidden from WGSL.
    let finfos = finfos.iter().filter(|finfo| !finfo.is_wgsl_hidden());

    let (field_attr_outers, field_attr_inners): (Vec<_>, Vec<_>) = finfos
        .clone()
        .map(|finfo| finfo.get_wgsl_attribute_pairs())
        .unzip();
    let field_names: Vec<_> = finfos.clone().map(|finfo| finfo.name.as_str()).collect();
    let field_types: Vec<_> = finfos.map(|finfo| finfo.ty.name()).collect();

    quote! {
        impl my_wgsl::ToIdent for #st_ident {
            fn to_ident() -> String {
                stringify!(#st_ident).to_owned()
            }
        }

        impl my_wgsl::AsStructure for #st_ident {
            fn as_structure() -> my_wgsl::Structure {
                my_wgsl::Structure {
                    ident: <Self as my_wgsl::ToIdent>::to_ident(),
                    members: vec![#(
                        my_wgsl::StructureMember {
                            attrs: my_wgsl::Attributes(vec![#(
                                my_wgsl::Attribute::from(
                                    (#field_attr_outers, #field_attr_inners)
                                )
                            ),*]),
                            ident: #field_names.to_owned(),
                            ty: #field_types.to_owned(),
                        }
                    ),*],
                }
            }
        }
    }
}

fn impl_from_for_u8_slice(st_ident: &Ident, st_size: usize) -> TokenStream2 {
    quote! {
        impl From<&#st_ident> for &[u8] {
            fn from(value: &#st_ident) -> Self {
                // Safety: Infallible.
                unsafe {
                    std::mem::transmute::<_, &[u8; #st_size]>(value)
                }
            }
        }
    }
}

fn validate_field(finfo: &FieldInfo, is_last: bool, is_uniform: bool) {
    // Validates field's size.
    if !is_last {
        assert!(
            finfo.size.is_some(),
            "cannot determine size of '{}'. all fields except the last one must be sized type. consider using #[size(n)]",
            finfo.name
        );
    }

    // Validates field's alignment.
    if finfo.align == FieldInfo::INIT_ALIGN {
        panic!(
            "cannot determine alignment of '{}'. consider using #[align(n)]",
            finfo.name
        );
    }

    // Users can set size and alignment manually by #[size(n)] and #[align(m)] respectively.
    // Therefore, we need to validate them.
    if finfo.is_array() {
        // If address space is uniform, then alignment of an array must be a multiple of 16.
        // ref: https://www.w3.org/TR/WGSL/#address-space-layout-constraints
        let align = finfo.align.get();
        if is_uniform {
            assert!(
                align % 16 == 0,
                "Uniform address space requires array to have a multiple of 16 alignment"
            );
        }
        // In array, array's size must be a multiple of array/element alignment.
        if let Some(size) = finfo.size {
            let size = size.get();
            assert!(
                size % align == 0,
                "size of fixed sized array must be a multiple of its alignment"
            );
        }
    }
}

// Compile time validation.
fn validate_wgsl_struct(
    finfos: &[FieldInfo],
    st_ident: &Ident,
    st_size: u32,
    st_align: u32,
    offsets: &[u32],
) -> TokenStream2 {
    let names: Vec<_> = finfos
        .iter()
        .map(|finfo| Ident::new(&finfo.name, Span::call_site()))
        .collect();
    let st_size_str = st_size.to_string(); // Removes suffix from something like 16u32.
    let st_align_str = st_align.to_string(); // Same as above.
    let is_last_zero_sized = finfos
        .iter()
        .last()
        .map(|finfo| finfo.size.is_none())
        .unwrap();
    quote! {
        const _: () = {
            // Validates offset of each field.
            #(
                assert!(
                    #offsets as usize == std::mem::offset_of!(#st_ident, #names),
                    concat!("offset of '", stringify!(#names), "' doesn't match. the upper field may need manual padding, which can be added with #[ignore]")
                );
            )*

            // Validates the whole structure's size
            // when we know the exact size of the structure.
            if !#is_last_zero_sized {
                assert!(
                    #st_size as usize == std::mem::size_of::<#st_ident>(),
                    concat!("calculated structure size(", #st_size_str, ") doesn't match")
                );
            }

            // Validates the whole structure's alignment.
            assert!(
                #st_align as usize == std::mem::align_of::<#st_ident>(),
                concat!("calculated structure alignment(", #st_align_str, ") doesn't match")
            );
        };
    }
}

#[derive(Debug, PartialEq, Eq)]
enum AddressSpace {
    Storage,
    Uniform,
}

impl AddressSpace {
    /// If the structure attribute doesn't have anything in it,
    /// [`Self::Storage`] is chosen as default.
    fn from(value: &StructAttribute) -> Self {
        let s;
        let attr = if let Some(ident) = value.ident.as_ref() {
            s = ident.to_string();
            s.as_str()
        } else {
            "Storage"
        };

        match attr {
            "Storage" => Self::Storage,
            "Uniform" => Self::Uniform,
            _ => panic!("Valid address space is one of Storage or Uniform"),
        }
    }
}

#[derive(Debug)]
struct FieldInfo {
    /// Field attributes.
    attrs: Vec<FieldAttribute>,

    /// Field name.
    name: String,

    /// Field type.
    ty: NestedType,

    /// Field size in bytes.
    /// Runtime sized array doesn't have its size.
    size: Option<NonZeroU32>,

    /// Field alignment in bytes.
    align: NonZeroU32,
}

impl FieldInfo {
    const INIT_ALIGN: NonZeroU32 = unsafe { NonZeroU32::new_unchecked(i32::MAX as u32) };

    fn from(field: &Field) -> Self {
        let mut this = Self {
            attrs: Vec::new(),
            name: field.ident.as_ref().unwrap().to_string(),
            ty: NestedType::default(),
            size: None,
            align: Self::INIT_ALIGN,
        };

        let attrs = field
            .attrs
            .iter()
            .filter_map(FieldAttribute::from)
            .collect::<Vec<_>>();
        for attr in attrs.iter() {
            match attr {
                FieldAttribute::Size(size) => {
                    this.set_size(*size);
                }
                FieldAttribute::Align(align) => {
                    this.set_align(*align);
                }
                FieldAttribute::WgslType(ty) => {
                    this.set_by_type(ty.clone());
                }
                FieldAttribute::Else(..) => {}
                FieldAttribute::Ignore => {}
            }
        }
        this.attrs = attrs;

        // If WGSL type is not determined by attributes, then infers it from the Rust type.
        if this.ty.name().is_empty() {
            let ty = NestedType::from_type(&field.ty);
            this.set_by_type(ty);
        }

        this
    }

    fn set_by_type(&mut self, ty: NestedType) {
        let ty_size = ty.size;
        let ty_align = ty.align;
        self.set_type(ty);

        // Inferred size can't overwrite.
        if let Some(size) = ty_size {
            if self.size.is_none() {
                self.set_size(size);
            }
        }

        // Inferred alignment can't overwrite.
        if let Some(align) = ty_align {
            if self.align == Self::INIT_ALIGN {
                self.set_align(align);
            }
        }
    }

    fn set_type(&mut self, ty: NestedType) {
        let this_name = self.ty.name();
        if !this_name.is_empty() {
            assert_eq!(this_name, ty.name(), "{}: type conflicts", this_name);
        }
        self.ty = ty;
    }

    fn set_size(&mut self, size: NonZeroU32) {
        self.size = Some(size);
    }

    fn set_align(&mut self, align: NonZeroU32) {
        assert!(
            [1 /* for padding */, 2, 4, 8, 16].contains(&align.get()),
            "align must be one of 2, 4, 8, or 16"
        );

        self.align = align;
    }

    fn get_wgsl_attribute_pairs(&self) -> (Vec<Cow<'_, str>>, Vec<Cow<'_, str>>) {
        self.attrs
            .iter()
            .filter_map(|attr| {
                if attr.is_wgsl_compatible() {
                    Some(attr.create_string_pair())
                } else {
                    None
                }
            })
            .unzip()
    }

    fn is_wgsl_hidden(&self) -> bool {
        self.attrs
            .iter()
            .any(|attr| matches!(attr, FieldAttribute::Ignore))
    }

    fn is_array(&self) -> bool {
        self.ty.is_array()
    }
}

#[derive(Debug)]
enum FieldAttribute {
    Size(NonZeroU32),
    Align(NonZeroU32),
    WgslType(NestedType),
    /// Outer attribute name and inner argumnet.
    Else(String, String),
    Ignore,
}

impl FieldAttribute {
    fn from(attr: &Attribute) -> Option<Self> {
        // #[size(n)]
        if attr.path().is_ident(AttributeKind::Size.as_str()) {
            let errmsg = "size expects a non zero number";
            let lit: LitInt = attr.parse_args().expect(errmsg);
            let v = lit.base10_parse::<u32>().expect(errmsg);
            let v = NonZeroU32::new(v).expect(errmsg);
            Some(Self::Size(v))
        }
        // #[align(n)]
        else if attr.path().is_ident(AttributeKind::Align.as_str()) {
            let errmsg = "align expects a non zero number";
            let lit: LitInt = attr.parse_args().expect(errmsg);
            let v = lit.base10_parse::<u32>().expect(errmsg);
            let v = NonZeroU32::new(v).expect(errmsg);
            Some(Self::Align(v))
        }
        // #[wgsl_type(type)]
        else if attr.path().is_ident(AttributeKind::WgslType.as_str()) {
            let ty = attr
                .parse_args_with(Type::parse)
                .expect("unrecognized wgsl type");
            Some(Self::WgslType(NestedType::from_type(&ty)))
        }
        // #[pad]
        else if attr.path().is_ident(AttributeKind::Ignore.as_str()) {
            Some(Self::Ignore)
        }
        // Comments following '///' look like attributes wrapped with #[doc..].
        else if attr.path().is_ident("doc") {
            None
        }
        // Other attributes like 'builtin' must be kept to generate WGSL struct.
        // So, turns them into string pair.
        else if let Some(outer) = attr.path().get_ident() {
            let outer = outer.to_string();

            let inner = if let Ok(ident) = attr.parse_args::<Ident>() {
                ident.to_string()
            } else if let Ok(lit) = attr.parse_args::<Lit>() {
                match lit {
                    Lit::Str(s) => s.value(),
                    Lit::Int(i) => i.base10_digits().to_owned(),
                    _ => panic!("unrecognized attribute argument inside {outer}"),
                }
            } else {
                panic!("unrecognized attribute argument inside {outer}");
            };

            Some(Self::Else(outer, inner))
        } else {
            None
        }
    }

    fn is_wgsl_compatible(&self) -> bool {
        match self {
            Self::Size(_) => true,
            Self::Align(_) => true,
            Self::WgslType(_) => false,
            Self::Else(..) => true,
            Self::Ignore => false,
        }
    }

    fn create_string_pair(&self) -> (Cow<'_, str>, Cow<'_, str>) {
        match self {
            Self::Size(size) => (
                Cow::Borrowed(AttributeKind::Size.as_str()),
                Cow::Owned(size.to_string()),
            ),
            Self::Align(align) => (
                Cow::Borrowed(AttributeKind::Align.as_str()),
                Cow::Owned(align.to_string()),
            ),
            Self::WgslType(ty) => (Cow::Borrowed(AttributeKind::WgslType.as_str()), ty.name()),
            Self::Else(outer, inner) => (Cow::Borrowed(outer), Cow::Borrowed(inner)),
            Self::Ignore => (
                Cow::Borrowed(AttributeKind::Ignore.as_str()),
                Cow::Borrowed(""),
            ),
        }
    }
}

#[derive(Debug)]
enum AttributeKind {
    Size,
    Align,
    WgslType,
    Ignore,
}

impl AttributeKind {
    fn as_str(&self) -> &str {
        match self {
            Self::Size => "size",
            Self::Align => "align",
            Self::WgslType => "wgsl_type",
            Self::Ignore => "ignore",
        }
    }
}

#[derive(Debug, Default, Clone)]
struct NestedType {
    /// Type name.
    outer: String,

    /// Comma seperated generic types inside brackets <..>.
    generics: Vec<Self>,

    /// Type size in bytes.
    size: Option<NonZeroU32>,

    /// Type alignment in bytes.
    align: Option<NonZeroU32>,
}

impl NestedType {
    fn from_type(ty: &Type) -> Self {
        match ty {
            Type::Path(type_path) => Self::from_path(&type_path.path),
            Type::Array(type_array) => Self::from_type_array(type_array),
            _ => panic!("not allowed Type"),
        }
    }

    fn from_path(path: &Path) -> Self {
        // We're considering WGSL struct only.
        // So we don't expect any double colons(::) in the path.
        assert!(path.leading_colon.is_none(), "double colons is not allowed");
        assert!(path.segments.len() < 2, "double colons is not allowed");

        Self::from_path_segment(&path.segments[0])
    }

    fn from_type_array(type_array: &TypeArray) -> Self {
        let elem = Self::from_type(&type_array.elem);
        let len = match &type_array.len {
            Expr::Lit(ExprLit {
                lit: Lit::Int(lit_int),
                ..
            }) => lit_int
                .base10_parse::<u32>()
                .expect("array length must be a number"),
            _ => panic!("array length must be a number"),
        };

        let outer = format!("[{}; {}]", elem.name(), len);
        let elem_size = elem.size.unwrap().get();
        let elem_align = elem.align.unwrap().get();
        let size = round_up_by_align(elem_align, elem_size * len);

        Self {
            outer,
            generics: Vec::new(),
            size: NonZeroU32::new(size),
            align: elem.align,
        }
    }

    fn from_path_segment(seg: &PathSegment) -> Self {
        let outer = seg.ident.to_string();
        let mut generics = Vec::new();
        match &seg.arguments {
            PathArguments::AngleBracketed(inside) => {
                for arg in inside.args.iter() {
                    generics.push(Self::from_generic_argument(arg));
                }
            }
            PathArguments::Parenthesized(..) => panic!("not allowed PathArgument"),
            PathArguments::None => (),
        }

        // Basic type: i32, u32, f32, f16, vec aliases, or mat aliases.
        let (size, align) = if let Some(size_align) = Self::basic_type_size_align(&outer) {
            let (size, align) = size_align;
            (NonZeroU32::new(size), NonZeroU32::new(align))
        }
        // vec generics: vec2/vec3/vec4 + <i32>/<u32>/<f32>/<f16>.
        else if outer.starts_with("vec") {
            // There must be only one generic parameter.
            assert_eq!(1, generics.len(), "vec must have one generic parameter");

            let (unit_size, _) = primitive_size_align(&generics[0].name());

            let (size, align) = vec_size_align_from_str(&outer[..4], unit_size);
            (NonZeroU32::new(size), NonZeroU32::new(align))
        }
        // mat generics: mat2x2/../mat4x4 + <i32>/<u32>/<f32>/<f16>.
        else if outer.starts_with("mat") {
            // There must be only one generic parameter.
            assert_eq!(1, generics.len(), "mat must have one generic parameter");

            let (unit_size, _) = primitive_size_align(&generics[0].name());

            let (size, align) = mat_size_align_from_str(&outer[..6], unit_size);
            (NonZeroU32::new(size), NonZeroU32::new(align))
        }
        // array generics.
        else if outer.starts_with("array") {
            // There must be element type.
            let elem_ty = generics
                .first()
                .expect("array must include element type inside brackets");

            // array<E, N>
            if let Some(elem_num) = generics.get(1) {
                let num = elem_num
                    .outer
                    .parse::<u32>()
                    .expect("second array generic parameter must be a non zero number");
                assert!(
                    num > 0,
                    "second array generic parameter must be a non zero number"
                );

                match (elem_ty.size, elem_ty.align) {
                    (Some(size), Some(align)) => {
                        let (size, align) = array_size_align(align.get(), size.get(), num);
                        (NonZeroU32::new(size), NonZeroU32::new(align))
                    }
                    _ => (None, None),
                }
            }
            // array<E>
            else {
                // This is runtime sized array, we can't know about the size of it.
                (None, elem_ty.align)
            }
        }
        // Unknown type like structure.
        else {
            // Unknown size and alignment.
            (None, None)
        };

        Self {
            outer,
            generics,
            size,
            align,
        }
    }

    fn from_generic_argument(arg: &GenericArgument) -> Self {
        match arg {
            GenericArgument::Type(ty) => Self::from_type(ty),
            GenericArgument::Const(expr) => Self::from_expr(expr),
            _ => panic!("not allowed GenericArgument"),
        }
    }

    fn from_expr(expr: &Expr) -> Self {
        match expr {
            Expr::Lit(expr_lit) => Self::from_lit(&expr_lit.lit),
            _ => panic!("not allowed Expr"),
        }
    }

    fn from_lit(lit: &Lit) -> Self {
        match lit {
            Lit::Int(lit_int) => Self {
                outer: lit_int.to_string(),
                generics: Vec::new(),
                size: None,
                align: None,
            },
            _ => panic!("not allowed Lit"),
        }
    }

    fn name(&self) -> Cow<'_, str> {
        if self.generics.is_empty() {
            Cow::Borrowed(&self.outer)
        } else {
            let mut name = self.outer.clone();
            name.push('<');
            for generic in self.generics.iter().take(self.generics.len() - 1) {
                name.push_str(&generic.name());
                name.push(',');
            }
            if let Some(generic) = self.generics.last() {
                name.push_str(&generic.name());
            }
            name.push('>');
            Cow::Owned(name)
        }
    }

    /// Determines size and align of the given basic type.  
    /// Basic types are,
    /// - i8 and u8 for padding.
    /// - i32, u32, f32, and f16.
    /// - vec aliases like vec2f, not generic like vec2<f32>.
    /// - mat aliases like mat2x2f, not generic like mat2x2<f32>.
    fn basic_type_size_align(ty: &str) -> Option<(u32, u32)> {
        let unit_type_to_size = |unit_type: char| -> u32 {
            match unit_type {
                'i' | 'u' | 'f' => 4,
                'h' => 2,
                _ => panic!("{} is invalid", ty),
            }
        };

        match ty {
            // i8 and u8 for padding.
            "i8" | "u8" => Some((1, 1)),
            // Primitive type i32, u32, f32, and f16.
            "i32" | "u32" | "f32" | "f16" => Some(primitive_size_align(ty)),
            // vec aliases.
            ty if ty.len() == 5 && ty.starts_with("vec") => {
                let unit_char = unsafe { ty.chars().nth(4).unwrap_unchecked() };
                let unit_size = unit_type_to_size(unit_char);
                Some(vec_size_align_from_str(&ty[..4], unit_size))
            }
            // mat aliases.
            ty if ty.len() == 7 && ty.starts_with("mat") => {
                let unit_char = unsafe { ty.chars().nth(6).unwrap_unchecked() };
                let unit_size = unit_type_to_size(unit_char);
                Some(mat_size_align_from_str(&ty[..6], unit_size))
            }
            _ => None,
        }
    }

    fn is_array(&self) -> bool {
        self.outer.as_str() == "array"
    }
}

#[derive(Debug)]
struct Repr {
    c: Option<()>,
    align: Option<NonZeroU32>,
    packed: Option<NonZeroU32>,
}

impl Repr {
    fn from(attrs: &[Attribute]) -> Self {
        let mut this = Self {
            c: None,
            align: None,
            packed: None,
        };

        for attr in attrs.iter() {
            if attr.path().is_ident("repr") {
                attr.parse_nested_meta(|meta| {
                    // #[repr(C)]
                    if meta.path.is_ident("C") {
                        this.c = Some(());
                        Ok(())
                    }
                    // #[repr(align)]
                    else if meta.path.is_ident("align") {
                        let content;
                        parenthesized!(content in meta.input);
                        let lit: LitInt = content.parse()?;
                        let n: u32 = lit.base10_parse()?;
                        this.align = NonZeroU32::new(n);
                        Ok(())
                    }
                    // #[repr(packed)] or #[repr(packed(n))]
                    else if meta.path.is_ident("packed") {
                        let n = if meta.input.peek(Paren) {
                            let content;
                            parenthesized!(content in meta.input);
                            let lit: LitInt = content.parse()?;
                            lit.base10_parse::<u32>()?
                        } else {
                            1
                        };
                        this.packed = NonZeroU32::new(n);
                        Ok(())
                    } else {
                        Ok(())
                    }
                })
                .unwrap();
            }
        }

        this
    }
}

struct StructAttribute {
    ident: Option<Ident>,
}

impl Parse for StructAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = if !input.is_empty() {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Self { ident })
    }
}

// ref: https://www.w3.org/TR/WGSL/#alignment-and-size
fn primitive_size_align(ty: &str) -> (u32, u32) {
    match ty {
        "i32" | "u32" | "f32" => (4, 4),
        "f16" => (2, 2),
        _ => panic!("{} is not a primitive type", ty),
    }
}

fn vec_size_align_from_str(s: &str, unit_size: u32) -> (u32, u32) {
    assert!(["vec2", "vec3", "vec4"].contains(&s), "{} is invalid", s);

    // Safety: Infallible.
    let dim = unsafe { s.chars().nth(3).unwrap().to_digit(10).unwrap_unchecked() };

    vec_size_align(dim, unit_size)
}

// ref: https://www.w3.org/TR/WGSL/#alignment-and-size
fn vec_size_align(dim: u32, unit_size: u32) -> (u32, u32) {
    let size = dim * unit_size;
    let align = size.next_power_of_two();
    (size, align)
}

fn mat_size_align_from_str(s: &str, unit_size: u32) -> (u32, u32) {
    assert!(
        [
            "mat2x2", "mat2x3", "mat2x4", "mat3x2", "mat3x3", "mat3x4", "mat4x2", "mat4x3",
            "mat4x4",
        ]
        .contains(&s),
        "{s} is invalid",
    );

    // Safety: Infallible.
    let nc = unsafe {
        s.chars()
            .nth(3)
            .unwrap_unchecked()
            .to_digit(10)
            .unwrap_unchecked()
    };
    let nr = unsafe {
        s.chars()
            .nth(5)
            .unwrap_unchecked()
            .to_digit(10)
            .unwrap_unchecked()
    };

    mat_size_align(nc, nr, unit_size)
}

// ref: https://www.w3.org/TR/WGSL/#alignment-and-size
fn mat_size_align(nc: u32, nr: u32, unit_size: u32) -> (u32, u32) {
    let (vec_r_size, vec_r_align) = vec_size_align(nr, unit_size);
    let (size, _) = array_size_align(vec_r_align, vec_r_size, nc);
    (size, vec_r_align)
}

// ref: https://www.w3.org/TR/WGSL/#alignment-and-size
fn array_size_align(elem_align: u32, elem_size: u32, num: u32) -> (u32, u32) {
    let size = num * round_up_by_align(elem_align, elem_size);
    (size, elem_align)
}

// ref: https://www.w3.org/TR/WGSL/#roundup
fn round_up_by_align(align: u32, value: u32) -> u32 {
    let mask = align - 1;
    (value + mask) & (!mask)
}

/// Implements `my_wgsl::ToIdent` and `my_wgsl::AsStructure` from literal struct.
///
/// # Examples
///
/// ```
/// use my_wgsl::*;
///
/// fn foo() {
///     let mut builder = Builder::new();
///     wgsl_decl_struct_from_str!(
///         "
///         struct PointLight {
///             position: vec3f,
///             color: vec3f,
///         }
///         "
///     );
///     wgsl_structs!(builder, PointLight);
/// }
/// ```
#[proc_macro]
pub fn wgsl_decl_struct_from_str(item: TokenStream) -> TokenStream {
    let item_lit = parse_macro_input!(item as LitStr);
    let value = item_lit.value();
    let item: TokenStream2 = value.parse().unwrap();
    let item: TokenStream = item.into();
    wgsl_decl_struct(TokenStream::new(), item)
}

/// Implements `my_wgsl::ToIdent` and `my_wgsl::AsStructure` for the struct.
///
/// # Examples
///
/// ```
/// use my_wgsl::*;
///
/// #[wgsl_decl_struct]
/// struct PointLight {
///     position: vec3f,
///     color: vec3f,
/// }
/// ```
#[proc_macro_attribute]
pub fn wgsl_decl_struct(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Doesn't allow any attributes.
    assert!(attr.is_empty(), "use this macro without attributes");

    let mut item_struct = parse_macro_input!(item as ItemStruct);
    let st_ident = &item_struct.ident;

    let mut finfos = Vec::new();
    for field in item_struct.fields.iter_mut() {
        finfos.push(FieldInfo::from(field));

        // Removes attributes.
        field.attrs.clear();
    }

    // Implements `my_wgsl::AsStructure`
    let impl_as_structure = impl_as_structure(st_ident, &finfos);

    TokenStream::from(quote! {
        #[allow(dead_code)] // Allows dead_code because we actually don't use this.
        #[allow(non_snake_case)] // Allows non snake case.
        #item_struct

        #impl_as_structure
    })
}

/// Generates `my_wgsl::Function` from the given literal.
///
/// # Examples
///
/// ```
/// use my_wgsl::*;
///
/// let mut builder = Builder::new();
/// let f = wgsl_decl_fn_from_str!(
///     "
///     #[fragment]
///     fn fragmentMain(#[location(0)] worldPos : vec3f,
///                     #[location(1)] normal : vec3f,
///                     #[location(2)] uv : vec2f)
///     -> #[location(0)] vec4f {
///         // Your code
///     }
///     "
/// );
/// builder.push_function(f);
/// ```
#[proc_macro]
pub fn wgsl_decl_fn_from_str(item: TokenStream) -> TokenStream {
    let item_lit = parse_macro_input!(item as LitStr);
    let value = item_lit.value();
    let item: TokenStream2 = value.parse().unwrap();
    let item: TokenStream = item.into();
    wgsl_decl_fn(item)
}

/// Generates `my_wgsl::Function` from the given code.
///
/// # Examples
///
/// ```
/// use my_wgsl::*;
///
/// let mut builder = Builder::new();
/// let f = wgsl_decl_fn!(
///     #[fragment]
///     fn fragmentMain(#[location(0)] worldPos : vec3f,
///                     #[location(1)] normal : vec3f,
///                     #[location(2)] uv : vec2f)
///     -> #[location(0)] vec4f {
///         // Your code
///     }
/// );
/// builder.push_function(f);
/// ```
#[proc_macro]
pub fn wgsl_decl_fn(item: TokenStream) -> TokenStream {
    let item_fn = parse_macro_input!(item as Function);
    let ident = &item_fn.ident;

    let (fn_attr_outer, fn_attr_inner) = attrs_to_string_pairs(&item_fn.attrs);

    let (input_attr_outers, input_attr_inners) = item_fn
        .inputs
        .iter()
        .map(fn_arg_to_attrs)
        .map(|attrs| attrs_to_string_pairs(attrs))
        .fold((vec![], vec![]), |mut acc, (outer, inner)| {
            acc.0.push(outer);
            acc.1.push(inner);
            acc
        });
    let input_idents = item_fn.inputs.iter().map(fn_arg_to_ident);
    let input_types = item_fn.inputs.iter().map(fn_arg_to_type);

    let output = if let Some((_, attrs, ty)) = &item_fn.output {
        let (outer, inner) = attrs_to_string_pairs(attrs);

        let ty = NestedType::from_type(ty);
        let ty_str = ty.name();

        quote! { Some(my_wgsl::FunctionParameter {
            attrs: my_wgsl::Attributes(vec![#(
                my_wgsl::Attribute::from((#outer, #inner))
            ),*]),
            ident: None,
            ty: #ty_str.to_owned(),
        })}
    } else {
        quote! { None }
    };

    let stmt = impl_comp_stat(&item_fn.stmt);

    let construct_function = quote! {{
        let attrs = my_wgsl::Attributes(vec![#(
            my_wgsl::Attribute::from(
                (#fn_attr_outer, #fn_attr_inner)
            )
        ),*]);

        let inputs = vec![#(
            my_wgsl::FunctionParameter {
                attrs: my_wgsl::Attributes(vec![#(
                    my_wgsl::Attribute::from(
                        (#input_attr_outers, #input_attr_inners)
                    )
                ),*]),
                ident: Some(stringify!(#input_idents).to_owned()),
                ty: stringify!(#input_types).to_owned(),
            }
        ),*];

        my_wgsl::Function {
            attrs,
            ident: stringify!(#ident).to_owned(),
            inputs,
            output: #output,
            stmt: #stmt,
        }
    }};

    TokenStream::from(construct_function)
}

#[allow(dead_code)]
fn impl_comp_stat(comp_stmt: &CompoundStatement) -> TokenStream2 {
    let (attr_outer, attr_inner) = attrs_to_string_pairs(&comp_stmt.attrs);

    let mut stmts = vec![];
    let mut others = vec![];
    for stmt in comp_stmt.stmts.iter() {
        if !matches!(stmt, Statement::Other(..)) && !others.is_empty() {
            stmts.extend(impl_others(&mut others));
        }
        match stmt {
            Statement::Compound(s) => {
                let comp_stat = impl_comp_stat(s);
                let tokens = quote! { my_wgsl::Statement::Compound(#comp_stat) };
                stmts.push(tokens);
            }
            Statement::Other(s) => others.push(s),
        }
    }

    if !others.is_empty() {
        stmts.extend(impl_others(&mut others));
    }

    quote! {
        my_wgsl::CompoundStatement {
            attrs: my_wgsl::Attributes(vec![#(
                my_wgsl::Attribute::from((#attr_outer, #attr_inner))
            ),*]),
            stmts: vec![ #( #stmts ),* ]
        }
    }
}

// This will disappear some day.
fn impl_others(others: &mut Vec<&String>) -> Vec<TokenStream2> {
    // A helper for pushing single statement commonly ends with ';'.
    fn push_single_stmt(stmt: &mut Vec<String>, res: &mut Vec<TokenStream2>) {
        if !stmt.is_empty() {
            let stmt_token = quote! {
                my_wgsl::Statement::Other(vec![#( String::from(#stmt) ),*])
            };
            res.push(stmt_token);
            stmt.clear();
        }
    }

    // Cuts.
    let mut res = vec![];
    let mut stmt = vec![];
    for &other in others.iter() {
        // Replaces some patterns inefficiently, anyway.
        let other = other.replace("+ +", "++");
        let other = other.replace(" ;", ";");

        let ch = other.chars().next();
        stmt.push(other);
        if ch == Some(';') {
            push_single_stmt(&mut stmt, &mut res);
        }
    }
    push_single_stmt(&mut stmt, &mut res);

    // We wrote others, empty it.
    others.clear();
    res
}

// Returns (outer, inner)
#[allow(dead_code)]
fn attrs_to_string_pairs(attrs: &[Attribute]) -> (Vec<String>, Vec<String>) {
    attrs
        .iter()
        .filter_map(|attr| {
            let (mut outer, mut inner) = (String::new(), String::new());
            attribute_to_string_pair(attr, &mut outer, &mut inner);
            (!outer.is_empty()).then_some((outer, inner))
        })
        .fold((vec![], vec![]), |mut acc, (outer, inner)| {
            acc.0.push(outer);
            acc.1.push(inner);
            acc
        })
}

/// Generates string from the given `Attribute`.
fn attribute_to_string_pair(attr: &Attribute, outer: &mut String, inner: &mut String) {
    match &attr.meta {
        Meta::Path(path) => {
            let ty = NestedType::from_path(path);
            outer.push_str(&ty.name());
        }
        Meta::List(meta_list) => meta_list_to_string_pair(meta_list, outer, inner),
        // Comment document belongs to here.
        Meta::NameValue(..) => {}
    }
}

/// Generates string from the given `MetaList`.
fn meta_list_to_string_pair(list: &MetaList, outer: &mut String, inner: &mut String) {
    let ty = NestedType::from_path(&list.path);
    outer.push_str(&ty.name());
    inner.push_str(&list.tokens.to_string());
}

fn fn_arg_to_attrs(arg: &FnArg) -> &Vec<Attribute> {
    match arg {
        FnArg::Typed(pat_type) => &pat_type.attrs,
        FnArg::Receiver(..) => panic!("not allowed FnArg"),
    }
}

fn fn_arg_to_ident(arg: &FnArg) -> &Ident {
    match arg {
        FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
            Pat::Ident(pat_ident) => &pat_ident.ident,
            _ => panic!("not allowed PatType"),
        },
        FnArg::Receiver(..) => panic!("not allowed FnArg"),
    }
}

fn fn_arg_to_type(arg: &FnArg) -> &Type {
    match arg {
        FnArg::Typed(pat_type) => pat_type.ty.as_ref(),
        FnArg::Receiver(..) => panic!("not allowed FnArg"),
    }
}

struct Function {
    attrs: Vec<Attribute>,
    _fn_token: Token![fn],
    ident: Ident,
    _paren_token: Paren,
    inputs: Punctuated<FnArg, Token![,]>,
    output: Option<(Token![->], Vec<Attribute>, Type)>,
    stmt: CompoundStatement,
}

impl Parse for Function {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Attributes.
        let attrs = input.call(Attribute::parse_outer)?;

        // Ident.
        let fn_token: Token![fn] = input.parse()?;
        let ident: Ident = input.parse()?;

        // Inputs.
        let content;
        let paren_token = parenthesized!(content in input);
        let inputs = content.parse_terminated(FnArg::parse, Token![,])?;

        // Output.
        let output = if input.peek(Token![->]) {
            let output_token: Token![->] = input.parse()?;
            let output_attr = input.call(Attribute::parse_outer)?;
            let output_type: Type = input.parse()?;
            Some((output_token, output_attr, output_type))
        } else {
            None
        };

        // Compound statement.
        let stmt: CompoundStatement = input.parse()?;

        Ok(Self {
            attrs,
            _fn_token: fn_token,
            ident,
            _paren_token: paren_token,
            inputs,
            output,
            stmt,
        })
    }
}

struct CompoundStatement {
    attrs: Vec<Attribute>,
    _brace_token: Brace,
    stmts: Vec<Statement>,
}

impl Parse for CompoundStatement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Attributes.
        let attrs = input.call(Attribute::parse_outer)?;

        // Block.
        let content;
        let brace_token = braced!(content in input); // Seems like it drops comments.
        let mut stmts = vec![];
        while !content.is_empty() {
            // Compound statement starts with attributes.
            // Compound statement without attributes.
            if content.peek(Token![#]) || content.peek(Brace) {
                stmts.push(Statement::Compound(content.parse()?));
            }
            // Other.
            else {
                move_to_punct(&content, &['#', '{'], &mut stmts);
            }
        }

        Ok(Self {
            attrs,
            _brace_token: brace_token,
            stmts,
        })
    }
}

// Can we take a token out from ParseBuffer itself?
// I don't know how to do, so let's use step() instead.
/// If we find one of targets during walking on the tokens,
/// returns without consuming found token.
/// All past tokens are kept in `buf` as Statement::Other(TokenTree2).
fn move_to_punct(input: &ParseBuffer, targets: &[char], buf: &mut Vec<Statement>) {
    input
        .step(|cursor| {
            let mut rest = *cursor;
            while let Some((tt, next)) = rest.token_tree() {
                match tt {
                    // Finds `targets` for punctuations.
                    TokenTree2::Punct(punct) if targets.iter().any(|&c| c == punct.as_char()) => {
                        return Ok(((), rest));
                    }
                    // If the `tt` is a `Group` like {..},
                    TokenTree2::Group(group) => {
                        match group.delimiter() {
                            // {..} is a sort of `CompoundStatement`, so parse along it.
                            Delimiter2::Brace => {
                                // `group.stream()` returns inner content of {..},
                                // so add {} again to parse as `CompoundStatement`.
                                let inner = group.stream();
                                let comp_stmt = TokenStream::from(quote! {{#inner}});
                                let parse = || {
                                    let c = parse_macro_input!(comp_stmt as CompoundStatement);
                                    buf.push(Statement::Compound(c));
                                    TokenStream::new()
                                };
                                if !parse().is_empty() {
                                    panic!("error during parsing Brace");
                                }
                            }
                            // Other types of groups are converted to string.
                            _ => {
                                buf.push(Statement::Other(group.to_string()));
                            }
                        }
                        rest = next;
                    }
                    _ => {
                        buf.push(Statement::Other(tt.to_string()));
                        rest = next;
                    }
                }
            }
            // Always moves the cursor by not returning Err.
            Ok(((), rest))
        })
        .unwrap()
}

enum Statement {
    Compound(CompoundStatement),
    Other(String), // TODO: implement as spec. some day.
}
