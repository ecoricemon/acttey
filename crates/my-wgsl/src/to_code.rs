pub(crate) const TAB_SIZE: usize = 2;

pub(crate) trait ConstructWgslCode {
    fn write_wgsl_code(&self, buf: &mut String);

    fn wgsl_code(&self) -> String {
        let mut buf = String::new();
        self.write_wgsl_code(&mut buf);
        buf
    }
}

pub(crate) trait ConstructPrettyCode: ConstructWgslCode {
    fn write_pretty_code(&self, buf: &mut String) {
        self.write_wgsl_code(buf)
    }
}

impl ConstructWgslCode for String {
    fn write_wgsl_code(&self, buf: &mut String) {
        buf.push_str(self);
    }
}

impl ConstructPrettyCode for String {}
