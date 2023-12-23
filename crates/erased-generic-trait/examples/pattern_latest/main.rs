use core::mem::{swap, zeroed};
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::Debug,
};

trait Element: 'static + Debug {}

trait Generic {
    fn generic<E: Element>(&mut self);
    // TODO: costs more. need this though?
    fn generic_writes<E: Element>(&mut self, input: E);
    fn generic_reads<E: Element>(&mut self, output: &mut E);
    fn non_generic(&self) -> &'static str;
}

trait ErasedGeneric {
    fn erased_generic(&mut self, ty_id: &TypeId);
    fn erased_generic_writes(&mut self, ty_id: &TypeId, input: &mut dyn Any);
    fn erased_generic_reads(&mut self, ty_id: &TypeId, output: &mut dyn Any);
    fn erased_non_generic(&self) -> &'static str;
}

struct Handler {
    fn_table: HandlerFnTable,
    v: Vec<Box<dyn Any>>, // Anonymous Vec
}

impl Generic for dyn ErasedGeneric {
    #[inline]
    fn generic<E: Element>(&mut self) {
        self.erased_generic(&TypeId::of::<E>())
    }

    #[inline]
    fn generic_writes<E: Element>(&mut self, input: E) {
        let mut input = input;
        self.erased_generic_writes(&TypeId::of::<E>(), &mut input)
    }

    #[inline]
    fn generic_reads<E: Element>(&mut self, output: &mut E) {
        self.erased_generic_reads(&TypeId::of::<E>(), output)
    }

    #[inline]
    fn non_generic(&self) -> &'static str {
        self.erased_non_generic()
    }
}

impl ErasedGeneric for Handler {
    #[inline]
    fn erased_generic(&mut self, ty_id: &TypeId) {
        let fn_table = self
            .fn_table
            .generic
            .take()
            .expect("fn_table_reads must be filled.");
        let delegator = fn_table
            .get(&ty_id)
            .expect("fn_table_reads doesn't have appropriate entry.");
        let ret = (delegator)(self);
        self.fn_table.generic = Some(fn_table);
        ret
    }

    #[inline]
    fn erased_generic_writes(&mut self, ty_id: &TypeId, input: &mut dyn Any) {
        let fn_table = self
            .fn_table
            .generic_writes
            .take()
            .expect("fn_table_writes must be filled.");
        let delegator = fn_table
            .get(&ty_id)
            .expect("fn_table_writes doesn't have appropriate entry.");
        let ret = (delegator)(self, input);
        self.fn_table.generic_writes = Some(fn_table);
        ret
    }

    #[inline]
    fn erased_generic_reads(&mut self, ty_id: &TypeId, output: &mut dyn Any) {
        let fn_table = self
            .fn_table
            .generic_reads
            .take()
            .expect("fn_table_reads must be filled.");
        let delegator = fn_table
            .get(&ty_id)
            .expect("fn_table_reads doesn't have appropriate entry.");
        let ret = (delegator)(self, output);
        self.fn_table.generic_reads = Some(fn_table);
        ret
    }

    #[inline]
    fn erased_non_generic(&self) -> &'static str {
        self.non_generic()
    }
}

impl Generic for Handler {
    fn generic<E: Element>(&mut self) {
        println!("generic() got an object of {:?}", TypeId::of::<E>());
    }

    fn generic_writes<E: Element>(&mut self, input: E) {
        self.v.push(Box::new(input));
        println!("generic_writes() got an object of {:?}", TypeId::of::<E>());
    }

    fn generic_reads<E: Element>(&mut self, output: &mut E) {
        let item = self.v.pop().unwrap();
        let casted = item.downcast::<E>().unwrap();
        *output = *casted;
    }

    fn non_generic(&self) -> &'static str {
        "Handler::foo()"
    }
}

type FnTableGeneric = HashMap<TypeId, Box<dyn Fn(&mut Handler)>, ahash::RandomState>;
type FnTableGenericWrites =
    HashMap<TypeId, Box<dyn Fn(&mut Handler, &mut dyn Any)>, ahash::RandomState>;
type FnTableGenericReads =
    HashMap<TypeId, Box<dyn Fn(&mut Handler, &mut dyn Any)>, ahash::RandomState>;

struct HandlerFnTable {
    generic: Option<FnTableGeneric>,
    generic_writes: Option<FnTableGenericWrites>,
    generic_reads: Option<FnTableGenericReads>,
}

impl HandlerFnTable {
    fn new() -> Self {
        Self {
            generic: Some(FnTableGeneric::default()),
            generic_writes: Some(FnTableGenericWrites::default()),
            generic_reads: Some(FnTableGenericReads::default()),
        }
    }

    #[allow(dead_code)]
    fn with<E: Element>(mut self) -> Self {
        self.add::<E>();
        self
    }

    #[allow(dead_code)]
    fn add<E: Element>(&mut self) -> &mut Self {
        if let Some(map) = self.generic.as_mut() {
            map.insert(
                TypeId::of::<E>(),
                Box::new(|handler: &mut Handler| {
                    handler.generic::<E>();
                }),
            );
        }
        if let Some(map) = self.generic_writes.as_mut() {
            map.insert(
                TypeId::of::<E>(),
                Box::new(|handler: &mut Handler, input: &mut dyn Any| {
                    // Worse than reference. Do I really need this?
                    let input_ref = input.downcast_mut::<E>().unwrap();
                    let mut input: E = unsafe { zeroed() };
                    swap(&mut input, input_ref);
                    handler.generic_writes::<E>(input);
                }),
            );
        }
        if let Some(map) = self.generic_reads.as_mut() {
            map.insert(
                TypeId::of::<E>(),
                Box::new(|handler: &mut Handler, output: &mut dyn Any| {
                    handler.generic_reads::<E>(output.downcast_mut::<E>().unwrap())
                }),
            );
        }
        self
    }
}

#[derive(Debug, PartialEq)]
struct A(u8);
#[derive(Debug, PartialEq)]
struct B(f32);

impl Element for A {}
impl Element for B {}

fn main() {
    // Constructs a trait object.
    let handler = Handler {
        fn_table: HandlerFnTable::new().with::<A>().with::<B>(),
        v: Vec::new(),
    };
    let mut trait_object: Box<dyn ErasedGeneric> = Box::new(handler);

    // Writes something in order to test the trait object.
    trait_object.generic_writes(A(123));
    trait_object.generic_writes(B(456.0));

    // Reads back.
    let mut a_read = A(0);
    let mut b_read = B(0.0);
    trait_object.generic_reads(&mut b_read);
    trait_object.generic_reads(&mut a_read);
    assert_eq!(A(123), a_read);
    assert_eq!(B(456.0), b_read);

    // Take a look at the printed types. They must be same with the types in generic methods.
    println!("Type A's id: {:?}", TypeId::of::<A>());
    println!("Type B's id: {:?}", TypeId::of::<B>());

    // How about generic without any parameters?
    trait_object.generic::<A>();
    trait_object.generic::<B>();

    // Non-generic method is also callable on the trait object.
    println!("{}", trait_object.non_generic());
}
