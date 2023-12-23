use core::mem::{swap, zeroed};
use erased_generic_trait::*;
use std::{
    any::{Any, TypeId},
    fmt::Debug,
};
mod generic;
use generic::*;

// `ErasedGeneric` here is the trait name you used.
// Other signatures must be exactly same with methods in the trait.
#[inject_fn_table(
    ErasedGeneric;
    fn generic_no_arg<E: Element>(&mut self);
    fn generic_writes<E: Element>(&mut self, param: &mut E);
    fn generic_reads<E: Element>(&mut self, param: &mut E);
    fn generic_multiple_arguments<E: Element>(
        &mut self,
        param1: &mut E,
        param2: &E,
        param3: i32
    ) -> i32;
    fn foo(&self) -> &'static str;
)]
struct Handler {
    v: Vec<Box<dyn Any>>, // Test Vec
}

// Your generic implementation.
impl Generic for Handler {
    fn generic_no_arg<E: Element>(&mut self) {
        println!("generic_no_arg() got an object of {:?}", TypeId::of::<E>());
    }

    fn generic_writes<E: Element>(&mut self, param: &mut E) {
        // Simple and unsafe writing test.
        let mut dummy: E = unsafe { zeroed() };
        swap(&mut dummy, param);
        let input = dummy;
        self.v.push(Box::new(input));

        println!("generic_writes() got an object of {:?}", TypeId::of::<E>());
    }

    fn generic_reads<E: Element>(&mut self, param: &mut E) {
        // Following reading test.
        let mut elem = self.v.pop().expect("There's no elements stacked.");
        if let Some(casted) = elem.downcast_mut::<E>() {
            swap(casted, param);
        }
    }

    fn generic_multiple_arguments<E: Element>(
        &mut self,
        _param1: &mut E,
        _param2: &E,
        param3: i32,
    ) -> i32 {
        param3 + 1
    }

    fn foo(&self) -> &'static str {
        "1234"
    }
}

// Test structs
#[derive(Debug, PartialEq)]
struct A(i32);
#[derive(Debug, PartialEq)]
struct B(u32);
#[derive(Debug, PartialEq)]
struct C(f32);
#[derive(Debug, PartialEq)]
struct D(char);

impl Element for A {}
impl Element for B {}
impl Element for C {}
impl Element for D {}

fn main() {
    // Let's make an instance that implements generic.
    let mut handler = Handler {
        // fn_table is injected by `inject_fn_table` macro.
        // You can omit A and B here.
        fn_table: generate_fn_table!(Handler, A, B),
        v: Vec::new(),
    };
    // We can add more entries before becoming a trait object.
    add_fn_table!(handler, C, D);

    // Constructs a trait object.
    // Currently, we can't add more entries using a trait object.
    let mut trait_object: Box<dyn ErasedGeneric> = Box::new(handler);

    // Writes something.
    trait_object.generic_writes(&mut A(0));
    trait_object.generic_writes(&mut B(1));
    trait_object.generic_writes(&mut C(2.0));
    trait_object.generic_writes(&mut D('3'));

    // Reads back.
    let mut a_read = A(0);
    let mut b_read = B(0);
    let mut c_read = C(0.0);
    let mut d_read = D('_');
    trait_object.generic_reads(&mut d_read);
    trait_object.generic_reads(&mut c_read);
    trait_object.generic_reads(&mut b_read);
    trait_object.generic_reads(&mut a_read);
    assert_eq!(A(0), a_read);
    assert_eq!(B(1), b_read);
    assert_eq!(C(2.0), c_read);
    assert_eq!(D('3'), d_read);

    // Calls methods with multiple arguments.
    let ret = trait_object.generic_multiple_arguments(&mut A(0), &A(0), 1);
    assert_eq!(2, ret);

    println!("Type A's id: {:?}", TypeId::of::<A>());
    println!("Type B's id: {:?}", TypeId::of::<B>());
    println!("Type C's id: {:?}", TypeId::of::<C>());
    println!("Type D's id: {:?}", TypeId::of::<D>());

    // Calls non-generic method.
    assert_eq!("1234", trait_object.foo());
}
