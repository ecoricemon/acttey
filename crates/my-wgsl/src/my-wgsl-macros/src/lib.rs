use proc_macro::TokenStream;
use proc_macro2::{Delimiter as Delimiter2, TokenStream as TokenStream2, TokenTree as TokenTree2};
use quote::quote;
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token::{Brace, Paren},
    Attribute, Expr, FnArg, GenericArgument, Ident, ItemStruct, Lit, MacroDelimiter, Meta,
    MetaList, Pat, Path, PathArguments, Token, Type, LitStr,
};

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
pub fn wgsl_decl_struct(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut item_struct = parse_macro_input!(item as ItemStruct);
    let ident = &item_struct.ident;

    let mut field_names = vec![];
    let mut field_types = vec![];
    let mut field_attr_outers = vec![];
    let mut field_attr_inners = vec![];
    let mut buf = String::new();
    for field in item_struct.fields.iter_mut() {
        // `field` is composed of attributes, name, and type, and looks like so,
        // #[builtin(vertex_index)] vi: u32

        // Collects attributes from a field.
        let (field_attr_outer, field_attr_inner) = attrs_to_string_pairs(&field.attrs);
        field.attrs.clear();

        // Extracts field name.
        buf.clear();
        ident_to_string(field.ident.as_ref(), &mut buf);
        let field_name = buf.clone();

        // Extracts field type.
        buf.clear();
        type_to_string(&field.ty, &mut buf);
        let field_type = buf.clone();

        // Keeps them for later use.
        field_names.push(field_name);
        field_types.push(field_type);
        field_attr_outers.push(field_attr_outer);
        field_attr_inners.push(field_attr_inner);
    }

    // Implements `my_wgsl::AsStructure`
    let impl_as_structure = quote! {
        impl my_wgsl::ToIdent for #ident {
            fn maybe_ident() -> &'static str {
                stringify!(#ident)
            }

            fn to_ident() -> String {
                Self::maybe_ident().to_owned()
            }
        }

        impl my_wgsl::AsStructure for #ident {
            fn as_structure() -> my_wgsl::Structure {
                let ident = Self::maybe_ident();
                let mut members = smallvec::smallvec![];
                #(
                    let attrs = my_wgsl::Attributes(smallvec::smallvec![#(
                        my_wgsl::Attribute::from(
                            (#field_attr_outers, #field_attr_inners)
                        )
                    ),*]);
                    members.push(my_wgsl::StructureMember {
                        attrs,
                        ident: #field_names,
                        ty: #field_types,
                    });
                )*
                my_wgsl::Structure { ident, members }
            }
        }
    };

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
        let mut ty_str = String::new();
        type_to_string(ty, &mut ty_str);

        quote! { Some(my_wgsl::FunctionParameter {
            attrs: my_wgsl::Attributes(smallvec::smallvec![#(
                my_wgsl::Attribute::from((#outer, #inner))
            ),*]),
            ident: None,
            ty: #ty_str,
        })}
    } else {
        quote! { None }
    };

    let stmt = impl_comp_stat(&item_fn.stmt);

    let construct_function = quote! {{
        let attrs = my_wgsl::Attributes(smallvec::smallvec![#(
            my_wgsl::Attribute::from(
                (#fn_attr_outer, #fn_attr_inner)
            )
        ),*]);

        let inputs = smallvec::smallvec![#(
            my_wgsl::FunctionParameter {
                attrs: my_wgsl::Attributes(smallvec::smallvec![#(
                    my_wgsl::Attribute::from(
                        (#input_attr_outers, #input_attr_inners)
                    )
                ),*]),
                ident: Some(stringify!(#input_idents)),
                ty: stringify!(#input_types),
            }
        ),*];

        my_wgsl::Function {
            attrs,
            ident: stringify!(#ident),
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
            attrs: my_wgsl::Attributes(smallvec::smallvec![#(
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
        .map(|attr| {
            let (mut outer, mut inner) = (String::new(), String::new());
            attribute_to_string_pair(attr, &mut outer, &mut inner);
            (outer, inner)
        })
        .fold((vec![], vec![]), |mut acc, (outer, inner)| {
            acc.0.push(outer);
            acc.1.push(inner);
            acc
        })
}

/// Generates string from the given `Attribute`.
#[allow(dead_code)]
fn attribute_to_string_pair(attr: &Attribute, outer: &mut String, inner: &mut String) {
    match &attr.meta {
        Meta::Path(path) => path_to_string(path, outer),
        Meta::List(meta_list) => meta_list_to_string_pair(meta_list, outer, inner),
        Meta::NameValue(..) => panic!("not allowed Meta"),
    }
}

/// Generates string from the given `MetaList`.
#[allow(dead_code)]
fn meta_list_to_string_pair(list: &MetaList, outer: &mut String, inner: &mut String) {
    path_to_string(&list.path, outer);
    inner.push_str(&list.tokens.to_string());
}

/// Generates string from the given `MetaList`.
#[allow(dead_code)]
fn meta_list_to_string(list: &MetaList, output: &mut String) {
    path_to_string(&list.path, output);
    let (open, close) = macro_delimiter_to_token_pair(&list.delimiter);
    output.push(open);
    output.push_str(&list.tokens.to_string());
    output.push(close);
}

/// Generates string from the given `Ident`.
#[allow(dead_code)]
fn ident_to_string(ident: Option<&Ident>, output: &mut String) {
    if let Some(ident) = ident {
        output.push_str(&ident.to_string());
    }
}

/// Generates string from the given `Type`.
#[allow(dead_code)]
fn type_to_string(ty: &Type, output: &mut String) {
    match ty {
        Type::Path(type_path) => path_to_string(&type_path.path, output),
        _ => unimplemented!(),
    }
}

/// Generates string from the given `Path`.
#[allow(dead_code)]
fn path_to_string(path: &Path, output: &mut String) {
    for (seg, sep) in path.segments.pairs().map(|pair| pair.into_tuple()) {
        ident_to_string(Some(&seg.ident), output);
        path_arguments_to_string(&seg.arguments, output);
        if sep.is_some() {
            output.push_str("::");
        }
    }
}

/// Generates string from the given `PathArguments`.
#[allow(dead_code)]
fn path_arguments_to_string(args: &PathArguments, output: &mut String) {
    match args {
        PathArguments::AngleBracketed(args) => {
            output.push('<');
            for arg in args.args.iter().take(args.args.len() - 1) {
                generic_argument_to_string(arg, output);
                output.push(',');
            }
            if let Some(last_arg) = args.args.last() {
                generic_argument_to_string(last_arg, output);
            }
            output.push('>');
        }
        PathArguments::Parenthesized(..) => panic!("not allowed PathArguments"),
        PathArguments::None => (),
    }
}

/// Generates string from the given `GenericArgument`.
#[allow(dead_code)]
fn generic_argument_to_string(arg: &GenericArgument, output: &mut String) {
    match arg {
        GenericArgument::Type(ty) => type_to_string(ty, output),
        GenericArgument::Const(expr) => expr_to_string(expr, output),
        _ => panic!("not allowed GenericArgument"),
    }
}

/// Generates string from the given `Expr`.
#[allow(dead_code)]
fn expr_to_string(expr: &Expr, output: &mut String) {
    match expr {
        Expr::Lit(expr_lit) => {
            lit_to_string(&expr_lit.lit, output);
        }
        _ => panic!("not allowed Expr"),
    }
}

/// Generates string from the given `Lit`.
#[allow(dead_code)]
fn lit_to_string(lit: &Lit, output: &mut String) {
    match lit {
        Lit::Int(lit_int) => {
            output.push_str(&lit_int.to_string());
        }
        _ => panic!("not allowed Lit"),
    }
}

/// Choose appropriate token pair for the given `MacroDelimiter`.
#[allow(dead_code)]
fn macro_delimiter_to_token_pair(del: &MacroDelimiter) -> (char, char) {
    match del {
        MacroDelimiter::Paren(..) => ('(', ')'),
        MacroDelimiter::Brace(..) => ('{', '}'),
        MacroDelimiter::Bracket(..) => ('[', ']'),
    }
}

#[allow(dead_code)]
fn fn_arg_to_attrs(arg: &FnArg) -> &Vec<Attribute> {
    match arg {
        FnArg::Typed(pat_type) => &pat_type.attrs,
        FnArg::Receiver(..) => panic!("not allowed FnArg"),
    }
}

#[allow(dead_code)]
fn fn_arg_to_ident(arg: &FnArg) -> &Ident {
    match arg {
        FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
            Pat::Ident(pat_ident) => &pat_ident.ident,
            _ => panic!("not allowed PatType"),
        },
        FnArg::Receiver(..) => panic!("not allowed FnArg"),
    }
}

#[allow(dead_code)]
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
