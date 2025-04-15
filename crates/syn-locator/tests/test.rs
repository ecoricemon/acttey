#[rustfmt::skip]

use syn_locator::*;
use pretty_assertions::assert_eq;
use std::{cell::Cell, pin::Pin};

#[test]
fn test_locate() {
    test_item_fn();
    test_item_struct();
    test_item_macro();
    test_field_pat();
}

fn test_item_fn() {
    let code = r"
    pub(crate) fn foo(a: i32, b: f32) -> usize {
        12
    }";
    let syn = syn::parse_str::<syn::ItemFn>(code).unwrap();
    let syn = Pin::new(&syn);
    syn.locate_as_entry(&unique_name(), code);

    assert_eq!(syn.vis.code(), "pub(crate)");
    assert_eq!(syn.sig.code(), "fn foo(a: i32, b: f32) -> usize");
    assert_eq!(
        syn.block.code(),
        r"{
        12
    }"
    );
}

fn test_item_struct() {
    let code = r"
    pub struct Foo<T: Bar> {
        a: T,
        b: i32,
    }";
    let syn = syn::parse_str::<syn::ItemStruct>(code).unwrap();
    let syn = Pin::new(&syn);
    syn.locate_as_entry(&unique_name(), code);

    assert_eq!(syn.vis.code(), "pub");
    assert_eq!(syn.struct_token.code(), "struct");
    assert_eq!(syn.ident.code(), "Foo");
    assert_eq!(syn.generics.code(), "<T: Bar>");
    assert_eq!(
        syn.fields.code(),
        r"{
        a: T,
        b: i32,
    }"
    );
}

fn test_item_macro() {
    let code = r"
    macro_rules! foo {
        ($id:ident) => { $id() };
    }";
    let syn = syn::parse_str::<syn::ItemMacro>(code).unwrap();
    let syn = Pin::new(&syn);
    syn.locate_as_entry(&unique_name(), code);

    assert_eq!(syn.ident.code(), "foo");
    assert_eq!(syn.mac.path.code(), "macro_rules");
    assert_eq!(syn.mac.bang_token.code(), "!");
    assert_eq!(
        syn.mac.delimiter.code(),
        r"{
        ($id:ident) => { $id() };
    }"
    );
}

fn test_field_pat() {
    let code = r"
    let X { 
        a, 
        ref b, 
        ref mut c,
        d: dd,
        e: ref ee,
        f: ref mut ff
    } = x;
    ";
    let syn = syn::parse_str::<syn::Stmt>(code).unwrap();
    let syn = Pin::new(&syn);
    syn.locate_as_entry(&unique_name(), code);

    let local = match syn.get_ref() {
        syn::Stmt::Local(v) => v,
        _ => unreachable!(),
    };

    let pat_struct = match &local.pat {
        syn::Pat::Struct(v) => v,
        _ => unreachable!(),
    };

    let field_pat = &pat_struct.fields[0];
    assert_eq!(field_pat.member.code(), "a");
    assert_eq!(field_pat.pat.code(), "a");
    let field_pat = &pat_struct.fields[1];
    assert_eq!(field_pat.member.code(), "b");
    assert_eq!(field_pat.pat.code(), "ref b");
    let field_pat = &pat_struct.fields[2];
    assert_eq!(field_pat.member.code(), "c");
    assert_eq!(field_pat.pat.code(), "ref mut c");
    let field_pat = &pat_struct.fields[3];
    assert_eq!(field_pat.member.code(), "d");
    assert_eq!(field_pat.pat.code(), "dd");
    let field_pat = &pat_struct.fields[4];
    assert_eq!(field_pat.member.code(), "e");
    assert_eq!(field_pat.pat.code(), "ref ee");
    let field_pat = &pat_struct.fields[5];
    assert_eq!(field_pat.member.code(), "f");
    assert_eq!(field_pat.pat.code(), "ref mut ff");
}

fn unique_name() -> String {
    thread_local! {
        static NUM: Cell<u32> = Cell::new(0);
    }

    let num = NUM.get();
    NUM.set(num + 1);
    num.to_string()
}
