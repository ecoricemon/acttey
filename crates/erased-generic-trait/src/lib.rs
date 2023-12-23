//! # Trait object from trait with generic methods
//!
//! ## When to use
//!
//! - When you want to make a trait obejct from a trait having generic methods.
//! - When generic methods have only one kind of bounds. (Please look at example below)
//! - When generic methods require 'static lifetime.
//! - When you can know all types that are passed to generic methods.
//!
//! ## Motivation
//!
//! In Rust, only object safe traits can become trait objects, generic methods make them not object safe.
//! To overcome this limitation, we can use `dyn Any` as parameters to take generic arguments
//! from non-generic methods, and call the generic methods in them.
//! We can inspect `TypeId`s from the 'dyn Ayn's, but we can't know concrete types from the 'TypeId's.
//! So macros are going to inject functions calling generic methods with the concrete types,
//! and invoke those functions according to the `TypeId`s.
//! But you should know this approach is not efficient.
//!
//! ## Usage
//!
//! You must use under macros all.
//! - erase_generic
//! - inject_fn_table
//! - generate_fn_table
//!
//! Optional macros.
//! - add_fn_table
//!
//! Please take a look at the example below.
//!
//! ## Example
//!
//! ```
//! use erased_generic_trait::*;
//!
//! // 'static is mandatory.
//! trait Element: 'static + std::fmt::Debug {}
//!
//! // `ErasedGeneric` here is the arbitrary trait name to be generated.
//! // Please put in any name you want.
//! #[erase_generic(ErasedGeneric)]
//! trait Generic {
//!     fn generic_writes<E: Element>(&mut self, param: &mut E);
//!     fn generic_reads<E: Element>(&mut self, param: &mut E);
//!     fn generic_multiple_arguments<E: Element>(
//!         &mut self,
//!         param1: &mut E,
//!         param2: &E,
//!         param3: i32,
//!     ) -> i32;
//!     fn foo(&self) -> &'static str;
//! }
//!
//! use core::mem::{swap, zeroed};
//! use std::{
//!     any::{Any, TypeId},
//!     fmt::Debug,
//! };
//!
//! // `ErasedGeneric` here is the trait name you used.
//! // Other signatures must be exactly same with methods in the trait.
//! #[inject_fn_table(
//!     ErasedGeneric;
//!     fn generic_writes<E: Element>(&mut self, param: &mut E);
//!     fn generic_reads<E: Element>(&mut self, param: &mut E);
//!     fn generic_multiple_arguments<E: Element>(
//!         &mut self,
//!         param1: &mut E,
//!         param2: &E,
//!         param3: i32
//!     ) -> i32;
//!     fn foo(&self) -> &'static str;
//! )]
//! struct Handler {
//!     v: Vec<Box<dyn Any>>, // Test Vec
//! }
//!
//! // Your generic implementation.
//! impl Generic for Handler {
//!     fn generic_writes<E: Element>(&mut self, param: &mut E) {
//!         // Simple and unsafe writing test.
//!         let mut dummy: E = unsafe { zeroed() };
//!         swap(&mut dummy, param);
//!         let input = dummy;
//!         self.v.push(Box::new(input));
//!
//!         println!("generic_writes() got an object of {:?}", TypeId::of::<E>());
//!     }
//!
//!     fn generic_reads<E: Element>(&mut self, param: &mut E) {
//!         // Following reading test.
//!         let mut elem = self.v.pop().expect("There's no elements stacked.");
//!         if let Some(casted) = elem.downcast_mut::<E>() {
//!             swap(casted, param);
//!         }
//!     }
//!
//!     fn generic_multiple_arguments<E: Element>(
//!         &mut self,
//!         _param1: &mut E,
//!         _param2: &E,
//!         param3: i32,
//!     ) -> i32 {
//!         param3 + 1
//!     }
//!
//!     fn foo(&self) -> &'static str {
//!         "1234"
//!     }
//! }
//!
//! // Test structs
//! #[derive(Debug, PartialEq)]
//! struct A(i32);
//! #[derive(Debug, PartialEq)]
//! struct B(u32);
//! #[derive(Debug, PartialEq)]
//! struct C(f32);
//! #[derive(Debug, PartialEq)]
//! struct D(char);
//!
//! impl Element for A {}
//! impl Element for B {}
//! impl Element for C {}
//! impl Element for D {}
//!
//! // Let's make an instance that implements generic.
//! let mut handler = Handler {
//!     // fn_table is injected by `inject_fn_table` macro.
//!     // You can omit A and B here.
//!     fn_table: generate_fn_table!(Handler, A, B),
//!     v: Vec::new(),
//! };
//! // We can add more entries before becoming a trait object.
//! add_fn_table!(handler, C, D);
//!
//! // Constructs a trait object.
//! // Currently, we can't add more entries using a trait object.
//! let mut trait_object: Box<dyn ErasedGeneric> = Box::new(handler);
//!
//! // Writes something.
//! trait_object.generic_writes(&mut A(0));
//! trait_object.generic_writes(&mut B(1));
//! trait_object.generic_writes(&mut C(2.0));
//! trait_object.generic_writes(&mut D('3'));
//!
//! // Reads back.
//! let mut a_read = A(0);
//! let mut b_read = B(0);
//! let mut c_read = C(0.0);
//! let mut d_read = D('_');
//! trait_object.generic_reads(&mut d_read);
//! trait_object.generic_reads(&mut c_read);
//! trait_object.generic_reads(&mut b_read);
//! trait_object.generic_reads(&mut a_read);
//! assert_eq!(A(0), a_read);
//! assert_eq!(B(1), b_read);
//! assert_eq!(C(2.0), c_read);
//! assert_eq!(D('3'), d_read);
//!
//! // Calls methods with multiple arguments.
//! let ret = trait_object.generic_multiple_arguments(
//!     &mut A(0),
//!     &A(0),
//!     1,
//! );
//! assert_eq!(2, ret);
//!
//! println!("Type A's id: {:?}", TypeId::of::<A>());
//! println!("Type B's id: {:?}", TypeId::of::<B>());
//! println!("Type C's id: {:?}", TypeId::of::<C>());
//! println!("Type D's id: {:?}", TypeId::of::<D>());
//!
//! // Calls non-generic method.
//! assert_eq!("1234", trait_object.foo());
//! ```
//!
//! ## Pattern explanation
//!
//! <https://github.com/ecoricemon/erased-generic-trait/blob/main/examples/pattern/main.rs>

use proc_macro::TokenStream;

mod add_fn_table;
mod common;
mod erase_generic;
mod generate_fn_table;
mod inject_fn_table;

/// Generates a new trait that doesn't have generic methods in it.
/// All generic methods are changed into non-generic methods,
/// which have names like `erased_foo()`.
/// Currently, supports only one generic parameter for all methods.
///
/// # Examples
///
/// ```
/// # use erased_generic_trait::*;
/// trait Element: 'static + std::fmt::Debug {}
///
/// // Put in a new trait name.
/// #[erase_generic(ErasedGeneric)]
/// trait Generic {
///     // Must receive &mut self for now.
///     // Must receive generic arguments as & or &mut for now.
///     fn generic<E: Element>(&mut self, param: &mut E);
/// }
/// ```
#[proc_macro_attribute]
pub fn erase_generic(attr: TokenStream, item: TokenStream) -> TokenStream {
    erase_generic::erase_generic(attr, item)
}

/// Injects a function table into the struct in order to dispatch generic methods dynamically.
/// Please put in the new trait name you used at the generic trait,
/// and method signatures of the generic trait.
///
/// # Examples
///
/// ```
/// # use erased_generic_trait::*;
/// # trait Element: 'static {}
/// # #[erase_generic(ErasedGeneric)]
/// # trait Generic {
/// #     fn generic<E: Element>(&mut self, param: &mut E);
/// # }
/// # impl Generic for Handler {
/// #   fn generic<E: Element>(&mut self, _param: &mut E) {}
/// # }
/// #[inject_fn_table(
///     ErasedGeneric;
///     fn generic<E: Element>(&mut self, param: &mut E);
/// )]
/// struct Handler {}
///
/// ```
#[proc_macro_attribute]
pub fn inject_fn_table(attr: TokenStream, item: TokenStream) -> TokenStream {
    inject_fn_table::inject_fn_table(attr, item)
}

/// Generates a new function table for you.
/// Please use this macro at the constuctors of your generic implementations.
///
/// # Examples
///
/// ```
/// # use erased_generic_trait::*;
/// # trait Element: 'static {}
/// # #[erase_generic(ErasedGeneric)]
/// # trait Generic {
/// #     fn generic<E: Element>(&mut self, param: &mut E);
/// # }
/// # impl Generic for Handler {
/// #   fn generic<E: Element>(&mut self, _param: &mut E) {}
/// # }
/// # #[inject_fn_table(
/// #     ErasedGeneric;
/// #     fn generic<E: Element>(&mut self, param: &mut E);
/// # )]
/// # struct Handler {}
/// # impl Element for A {}
/// # impl Element for B {}
/// // Assumes that these structs will be passed into generic methods.
/// struct A;
/// struct B;
///
/// // Assumes that `Handler` here is a implementation of your generic trait.
/// let handler = Handler {
///     // fn_table is injected by `inject_fn_table` macro.
///     fn_table: generate_fn_table!(Handler, A,B),
///     // Else you need to initialize.
/// };
/// ```
#[proc_macro]
pub fn generate_fn_table(input: TokenStream) -> TokenStream {
    generate_fn_table::generate_fn_table(input)
}

/// Adds new entries into a function table for you.
/// You can use this before becoming a trait object.
///
/// # Examples
///
/// ```
/// # use erased_generic_trait::*;
/// # trait Element: 'static {}
/// # #[erase_generic(ErasedGeneric)]
/// # trait Generic {
/// #     fn generic<E: Element>(&mut self, param: &mut E);
/// # }
/// # impl Generic for Handler {
/// #   fn generic<E: Element>(&mut self, _param: &mut E) {}
/// # }
/// # #[inject_fn_table(
/// #     ErasedGeneric;
/// #     fn generic<E: Element>(&mut self, param: &mut E);
/// # )]
/// # struct Handler {}
/// # impl Element for A {}
/// # impl Element for B {}
/// // Assumes that these structs will be passed into generic methods.
/// struct A;
/// struct B;
///
/// // Assumes that `Handler` here is a implementation of your generic trait.
/// let mut handler = Handler {
///     // fn_table is injected by `inject_fn_table` macro.
///     fn_table: generate_fn_table!(Handler),
///     // Else you need to initialize.
/// };
/// add_fn_table!(handler, A, B);
/// ```
#[proc_macro]
pub fn add_fn_table(input: TokenStream) -> TokenStream {
    add_fn_table::add_fn_table(input)
}
