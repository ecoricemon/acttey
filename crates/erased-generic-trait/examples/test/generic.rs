use erased_generic_trait::*;

// 'static is mandatory.
pub trait Element: 'static + std::fmt::Debug {}

// `ErasedGeneric` here is the arbitrary trait name to be generated.
// Please put in any name you want.
#[erase_generic(ErasedGeneric)]
pub trait Generic {
    fn generic_no_arg<E: Element>(&mut self);
    fn generic_writes<E: Element>(&mut self, param: &mut E);
    fn generic_reads<E: Element>(&mut self, param: &mut E);
    fn generic_multiple_arguments<E: Element>(
        &mut self,
        param1: &mut E,
        param2: &E,
        param3: i32,
    ) -> i32;
    fn foo(&self) -> &'static str;
}
