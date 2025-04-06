#[rustfmt::skip]

use syn_locator::*;
use pretty_assertions::assert_eq;
use std::pin::Pin;

#[test]
fn test_locate() {
    test_item_fn();
    test_item_struct();
}

fn test_item_fn() {
    let code = r"
    pub(crate) fn foo(a: i32, b: f32) -> usize {
        12
    }";
    let syn = syn::parse_str::<syn::ItemFn>(code).unwrap();
    let syn = Pin::new(&syn);
    syn.locate_as_entry("test_item_fn", code);

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
    syn.locate_as_entry("test_item_struct", code);

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
