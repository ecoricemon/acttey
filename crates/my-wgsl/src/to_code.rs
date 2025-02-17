pub(crate) const TAB_SIZE: usize = 2;

pub trait Identify {
    fn ident() -> String;
}

pub(crate) trait PutStr {
    fn put_ident(&self, buf: &mut String);

    fn put_str(&self, buf: &mut String);

    fn to_str(&self) -> String {
        let mut buf = String::new();
        self.put_str(&mut buf);
        buf
    }
}

pub(crate) trait PutStrPretty: PutStr {
    fn put_ident_pretty(&self, buf: &mut String) {
        self.put_ident(buf)
    }

    fn put_str_pretty(&self, buf: &mut String) {
        self.put_str(buf)
    }
}

impl PutStr for String {
    fn put_ident(&self, buf: &mut String) {
        buf.push_str(self);
    }

    fn put_str(&self, buf: &mut String) {
        self.put_ident(buf);
    }
}

impl PutStrPretty for String {}
