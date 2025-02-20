use proc_macro2::TokenStream as TokenStream2;

pub(crate) trait RuntimeWgslToken {
    fn runtime_tokens(&self) -> TokenStream2;
}

pub(crate) trait ComptimeWgslCode {
    fn write_wgsl_code(&self, buf: &mut String);
}
