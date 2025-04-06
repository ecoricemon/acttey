#![doc = include_str!("../README.md")]

use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    fmt,
    pin::Pin,
};

pub trait LocateEntry: Locate {
    fn locate_as_entry(self: Pin<&Self>, file_path: &str, code: &str) {
        let loc = self.location(file_path, code);
        self.locate(loc.file_path, code, 0);
    }

    #[doc(hidden)]
    fn location(self: Pin<&Self>, file_path: &str, code: &str) -> Location {
        LOCATOR.with_borrow_mut(|locator| locator.insert_file(&*self, file_path, code))
    }
}

impl<T: Locate> LocateEntry for T {}

pub trait Locate: Any {
    // impl
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location;

    fn relocate(&self, loc: Location) {
        LOCATOR.with_borrow_mut(|locator| {
            locator.set_location(self, loc);
        });
    }

    // call from parent node
    fn locate(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let loc = self.find_loc(file_path, code, offset);
        LOCATOR.with_borrow_mut(|locator| {
            locator.set_location(self, loc);
        });
        loc
    }

    fn location(&self) -> Location
    where
        Self: fmt::Debug,
    {
        LOCATOR
            .with_borrow(|locator| locator.get_location(self))
            .unwrap_or_else(|| {
                panic!(
                    "failed to find the location of `{self:?}`. did you forget `Locate::locate`?"
                )
            })
    }

    fn location_message(&self) -> String
    where
        Self: fmt::Debug,
    {
        LOCATOR
            .with_borrow(|locator| {
                let loc = locator.get_location(self)?;
                let path = loc.file_path;
                let code = locator.get_code(path)?;
                let line = code.as_bytes()[..loc.start]
                    .iter()
                    .filter(|&&b| b == b'\n')
                    .count()
                    + 1;
                let content = &code[loc.start..loc.end];

                Some(format!("{path}:{line}: {content}"))
            })
            .unwrap_or_else(|| {
                panic!(
                    "failed to find the location of `{self:?}`. did you forget `Locate::locate`?"
                )
            })
    }

    fn code(&self) -> String
    where
        Self: fmt::Debug,
    {
        LOCATOR
            .with_borrow(|locator| {
                let loc = locator.get_location(self)?;
                let path = loc.file_path;
                let code = locator.get_code(path)?;
                let content = &code[loc.start..loc.end];

                Some(content.to_owned())
            })
            .unwrap_or_else(|| {
                panic!(
                    "failed to find the location of `{self:?}`. did you forget `Locate::locate`?"
                )
            })
    }
}

pub trait LocateGroup {
    fn locate_as_group(&self, file_path: &'static str, code: &str, offset: usize) -> Location;
    fn relocate_as_group(&self, loc: Location);
}

macro_rules! impl_locate_group {
    ( $($i:expr),* ; $($ri:expr),* ) => {
        paste::paste! {
            impl<'a, $([<A $i>]: Locate),*> LocateGroup for ( $( &'a [<A $i>] ),* ) {
                #[allow(unused_assignments)]
                fn locate_as_group(&self, file_path: &'static str, code: &str, offset: usize)
                    -> Location
                {
                    let ( $( [<this $i>] ),* ) = self;

                    // Calls locate() on children.
                    let mut end = offset;
                    $(
                        let [<loc $i>] = [<this $i>].locate(file_path, code, end);
                        end = [<loc $i>].end;
                    )*

                    // Determines start.
                    let mut start = usize::MAX;
                    $(
                        if [<loc $i>].start != [<loc $i>].end {
                            start = start.min( [<loc $i>].start );
                        }
                    )*
                    if start == usize::MAX {
                        start = offset;
                    }

                    // Relocates empty children to the closest non-empty child.
                    let mut cur = end;
                    $(
                        if [<loc $ri>].start == [<loc $ri>].end {
                            [<this $ri>].relocate(Location {
                                file_path,
                                start: cur,
                                end: cur
                            });
                        } else {
                            cur = [<loc $ri>].start;
                        }
                    )*

                    Location {
                        file_path,
                        start,
                        end
                    }
                }

                fn relocate_as_group(&self, loc: Location) {
                    let ( $( [<this $i>] ),* ) = self;

                    // Calls relocate() on children.
                    $(
                        [<this $i>].relocate(loc);
                    )*
                }
            }
        }
    };
}

impl LocateGroup for () {
    fn locate_as_group(&self, file_path: &'static str, _code: &str, offset: usize) -> Location {
        Location {
            file_path,
            start: offset,
            end: offset,
        }
    }

    fn relocate_as_group(&self, _: Location) {}
}

impl<T: Locate> LocateGroup for &T {
    fn locate_as_group(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        self.locate(file_path, code, offset)
    }

    fn relocate_as_group(&self, loc: Location) {
        self.relocate(loc)
    }
}

impl_locate_group!(0, 1 ; 1, 0);
impl_locate_group!(0, 1, 2 ; 2, 1, 0);
impl_locate_group!(0, 1, 2, 3 ; 3, 2, 1, 0);
impl_locate_group!(0, 1, 2, 3, 4 ; 4, 3, 2, 1, 0);
impl_locate_group!(0, 1, 2, 3, 4, 5 ; 5, 4, 3, 2, 1, 0);
impl_locate_group!(0, 1, 2, 3, 4, 5, 6 ; 6, 5, 4, 3, 2, 1, 0);
impl_locate_group!(0, 1, 2, 3, 4, 5, 6, 7 ; 7, 6, 5, 4, 3, 2, 1, 0);
impl_locate_group!(0, 1, 2, 3, 4, 5, 6, 7, 8 ; 8, 7, 6, 5, 4, 3, 2, 1, 0);
impl_locate_group!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ; 9, 8, 7, 6, 5, 4, 3, 2, 1, 0);
impl_locate_group!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ; 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0);

pub struct Surround<'s, F, S, I, B> {
    pub front: F,
    pub surround: &'s S,
    pub inner: I,
    pub back: B,
}

impl<F, S, I, B> Surround<'_, F, S, I, B>
where
    F: LocateGroup,
    S: Locate,
    I: LocateGroup,
    B: LocateGroup,
{
    pub fn locate(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        // Calls locate() on fields.
        let front_loc = self.front.locate_as_group(file_path, code, offset);
        let surround_loc = self.surround.locate(file_path, code, front_loc.end);
        self.inner
            .locate_as_group(file_path, code, surround_loc.start + 1);
        let back_loc = self.back.locate_as_group(file_path, code, surround_loc.end);

        // Relocates front if needed
        let mut start = front_loc.start;
        if front_loc.start == front_loc.end {
            self.front.relocate_as_group(Location {
                file_path,
                start: surround_loc.start,
                end: surround_loc.start,
            });
            start = surround_loc.start;
        }

        // Relocates back if needed
        let mut end = back_loc.end;
        if back_loc.start == back_loc.end {
            self.back.relocate_as_group(Location {
                file_path,
                start: surround_loc.end,
                end: surround_loc.end,
            });
            end = surround_loc.end;
        }

        Location {
            file_path,
            start,
            end,
        }
    }
}

pub struct Qualified<'a, F, B> {
    pub front: F,
    pub qself: &'a syn::QSelf,
    pub path: &'a syn::Path,
    pub back: B,
}

impl<F, B> Qualified<'_, F, B>
where
    F: LocateGroup,
    B: LocateGroup,
{
    pub fn locate(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        // Calls locate() on fields.
        let front_loc = self.front.locate_as_group(file_path, code, offset);

        let qself_loc = self.qself.locate(file_path, code, front_loc.end);
        let qself_mid_loc = self.qself.as_token.location();

        // Path will be evaluated on something like `a::b::Trait>::Assoc`. The
        // string contains '>' though, it would be fine because we will skip it
        // during string matching.
        let path_loc = self.path.locate(file_path, code, qself_mid_loc.end);

        let back_loc = self.back.locate_as_group(file_path, code, path_loc.end);

        // Relocates front if needed
        let mut start = front_loc.start;
        if front_loc.start == front_loc.end {
            self.front.relocate_as_group(Location {
                file_path,
                start: qself_loc.start,
                end: qself_loc.start,
            });
            start = qself_loc.start;
        }

        // Relocates back if needed
        let mut end = back_loc.end;
        if back_loc.start == back_loc.end {
            self.back.relocate_as_group(Location {
                file_path,
                start: path_loc.end,
                end: path_loc.end,
            });
            end = path_loc.end;
        }

        Location {
            file_path,
            start,
            end,
        }
    }
}

pub struct Locator {
    files: HashMap<&'static str, File>,
    map: HashMap<LocationKey, Location>,
}

impl Locator {
    fn new() -> Self {
        Self {
            files: HashMap::new(),
            map: HashMap::new(),
        }
    }

    /// Inserts file then returns its index.
    fn insert_file<T: Any + ?Sized>(
        &mut self,
        syn_file: &T,
        file_path: &str,
        code: &str,
    ) -> Location {
        fn inner(this: &mut Locator, key: LocationKey, file_path: &str, code: &str) -> Location {
            if let Some(loc) = this.map.get(&key) {
                return *loc;
            }
            if this.files.contains_key(file_path) {
                panic!("duplicate `{file_path}`");
            }

            let file_path: Box<str> = file_path.into();
            let file_path = Box::leak(file_path);
            this.files.insert(file_path, File::new(code));

            let loc = Location {
                file_path,
                start: 0,
                end: code.len(),
            };
            this.map.insert(key, loc);

            loc
        }

        inner(self, LocationKey::new(syn_file), file_path, code)
    }

    fn set_location<T: Any + ?Sized>(&mut self, syn_node: &T, loc: Location) {
        self.map.insert(LocationKey::new(syn_node), loc);
    }

    fn get_location<T: Any + ?Sized>(&self, syn_node: &T) -> Option<Location> {
        self.map.get(&LocationKey::new(syn_node)).cloned()
    }

    fn get_code(&self, file_path: &str) -> Option<&str> {
        self.files.get(file_path).map(|file| file.code.as_str())
    }
}

impl Drop for Locator {
    fn drop(&mut self) {
        for (file_path, _) in self.files.drain() {
            let ptr = file_path as *const str as *mut str;
            unsafe { drop(Box::from_raw(ptr)) };
        }
    }
}

struct File {
    /// Filtered Rust code.
    code: String,
}

impl File {
    fn new(code: &str) -> Self {
        Self {
            code: Self::remove_non_tokens(code),
        }
    }

    /// Replaces comments with white spaces from the given code for further
    /// token matching.
    fn remove_non_tokens(code: &str) -> String {
        use regex::{Captures, Regex};

        // Regex doesn't support recursion, so that we cannot remove nested
        // comments. We need to write code from scratch to do that.
        thread_local! {
            static RE: Regex = Regex::new(r#"(?x)
                (//[^\n]*)  # Single line comment
                |
                (?s)
                (/\*.*?\*/) # Block comment (Recursion is not supported)
            "#).unwrap();
        }

        RE.with(|re| re.replace_all(code, |caps: &Captures| " ".repeat(caps[0].len())))
            .into_owned()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocationKey {
    ptr: *const (),

    // Because pointer of a struct is the same as pointer of the first child of
    // it, we distinguash them using type id.
    ty: TypeId,
}

impl LocationKey {
    fn new<T: Any + ?Sized>(t: &T) -> Self {
        Self {
            ptr: t as *const T as *const (),
            ty: TypeId::of::<T>(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location {
    pub file_path: &'static str,

    /// Byte index to the code.
    pub start: usize,

    /// Byte index to the code. Exclusive
    pub end: usize,
}

thread_local! {
    static LOCATOR: RefCell<Locator> = RefCell::new(Locator::new());
}

macro_rules! impl_locate_for_token {
    ($ty:ty, $token:literal, char) => {
        impl Locate for $ty {
            fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
                helper::char_location(file_path, code, offset, $token)
            }
        }
    };
    ($ty:ty, $token:literal, str) => {
        impl Locate for $ty {
            fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
                helper::str_location(file_path, code, offset, $token)
            }
        }
    };
}

impl_locate_for_token!(syn::Token![abstract], "abstract", str);
impl_locate_for_token!(syn::Token![as], "as", str);
impl_locate_for_token!(syn::Token![async], "async", str);
impl_locate_for_token!(syn::Token![auto], "auto", str);
impl_locate_for_token!(syn::Token![await], "await", str);
impl_locate_for_token!(syn::Token![become], "become", str);
impl_locate_for_token!(syn::Token![box], "box", str);
impl_locate_for_token!(syn::Token![break], "break", str);
impl_locate_for_token!(syn::Token![const], "const", str);
impl_locate_for_token!(syn::Token![continue], "continue", str);
impl_locate_for_token!(syn::Token![crate], "crate", str);
impl_locate_for_token!(syn::Token![default], "default", str);
impl_locate_for_token!(syn::Token![do], "do", str);
impl_locate_for_token!(syn::Token![dyn], "dyn", str);
impl_locate_for_token!(syn::Token![else], "else", str);
impl_locate_for_token!(syn::Token![enum], "enum", str);
impl_locate_for_token!(syn::Token![extern], "extern", str);
impl_locate_for_token!(syn::Token![final], "final", str);
impl_locate_for_token!(syn::Token![fn], "fn", str);
impl_locate_for_token!(syn::Token![for], "for", str);
impl_locate_for_token!(syn::Token![if], "if", str);
impl_locate_for_token!(syn::Token![impl], "impl", str);
impl_locate_for_token!(syn::Token![in], "in", str);
impl_locate_for_token!(syn::Token![let], "let", str);
impl_locate_for_token!(syn::Token![loop], "loop", str);
impl_locate_for_token!(syn::Token![macro], "macro", str);
impl_locate_for_token!(syn::Token![match], "match", str);
impl_locate_for_token!(syn::Token![mod], "mod", str);
impl_locate_for_token!(syn::Token![move], "move", str);
impl_locate_for_token!(syn::Token![mut], "mut", str);
impl_locate_for_token!(syn::Token![override], "override", str);
impl_locate_for_token!(syn::Token![priv], "priv", str);
impl_locate_for_token!(syn::Token![pub], "pub", str);
impl_locate_for_token!(syn::Token![raw], "raw", str);
impl_locate_for_token!(syn::Token![ref], "ref", str);
impl_locate_for_token!(syn::Token![return], "return", str);
impl_locate_for_token!(syn::Token![Self], "Self", str);
impl_locate_for_token!(syn::Token![self], "self", str);
impl_locate_for_token!(syn::Token![static], "static", str);
impl_locate_for_token!(syn::Token![struct], "struct", str);
impl_locate_for_token!(syn::Token![super], "super", str);
impl_locate_for_token!(syn::Token![trait], "trait", str);
impl_locate_for_token!(syn::Token![try], "try", str);
impl_locate_for_token!(syn::Token![type], "type", str);
impl_locate_for_token!(syn::Token![typeof], "typeof", str);
impl_locate_for_token!(syn::Token![union], "union", str);
impl_locate_for_token!(syn::Token![unsafe], "unsafe", str);
impl_locate_for_token!(syn::Token![unsized], "unsized", str);
impl_locate_for_token!(syn::Token![use], "use", str);
impl_locate_for_token!(syn::Token![virtual], "virtual", str);
impl_locate_for_token!(syn::Token![where], "where", str);
impl_locate_for_token!(syn::Token![while], "while", str);
impl_locate_for_token!(syn::Token![yield], "yield", str);
impl_locate_for_token!(syn::Token![&], '&', char);
impl_locate_for_token!(syn::Token![&&], "&&", str);
impl_locate_for_token!(syn::Token![&=], "&=", str);
impl_locate_for_token!(syn::Token![@], '@', char);
impl_locate_for_token!(syn::Token![^], '^', char);
impl_locate_for_token!(syn::Token![^=], "^=", str);
impl_locate_for_token!(syn::Token![:], ':', char);
impl_locate_for_token!(syn::Token![,], ',', char);
impl_locate_for_token!(syn::Token![$], '$', char);
impl_locate_for_token!(syn::Token![.], '.', char);
impl_locate_for_token!(syn::Token![..], "..", str);
impl_locate_for_token!(syn::Token![...], "...", str);
impl_locate_for_token!(syn::Token![..=], "..=", str);
impl_locate_for_token!(syn::Token![=], '=', char);
impl_locate_for_token!(syn::Token![==], "==", str);
impl_locate_for_token!(syn::Token![=>], "=>", str);
impl_locate_for_token!(syn::Token![>=], ">=", str);
impl_locate_for_token!(syn::Token![>], '>', char);
impl_locate_for_token!(syn::Token![<-], "<-", str);
impl_locate_for_token!(syn::Token![<=], "<=", str);
impl_locate_for_token!(syn::Token![<], '<', char);
impl_locate_for_token!(syn::Token![-], '-', char);
impl_locate_for_token!(syn::Token![-=], "-=", str);
impl_locate_for_token!(syn::Token![!=], "!=", str);
impl_locate_for_token!(syn::Token![!], '!', char);
impl_locate_for_token!(syn::Token![|], '|', char);
impl_locate_for_token!(syn::Token![|=], "|=", str);
impl_locate_for_token!(syn::Token![||], "||", str);
impl_locate_for_token!(syn::Token![::], "::", str);
impl_locate_for_token!(syn::Token![%], '%', char);
impl_locate_for_token!(syn::Token![%=], "%=", str);
impl_locate_for_token!(syn::Token![+], '+', char);
impl_locate_for_token!(syn::Token![+=], "+=", str);
impl_locate_for_token!(syn::Token![#], '#', char);
impl_locate_for_token!(syn::Token![?], '?', char);
impl_locate_for_token!(syn::Token![->], "->", str);
impl_locate_for_token!(syn::Token![;], ';', char);
impl_locate_for_token!(syn::Token![<<], "<<", str);
impl_locate_for_token!(syn::Token![<<=], "<<=", str);
impl_locate_for_token!(syn::Token![>>], ">>", str);
impl_locate_for_token!(syn::Token![>>=], ">>=", str);
impl_locate_for_token!(syn::Token![/], '/', char);
impl_locate_for_token!(syn::Token![/=], "/=", str);
impl_locate_for_token!(syn::Token![*], '*', char);
impl_locate_for_token!(syn::Token![*=], "*=", str);
impl_locate_for_token!(syn::Token![~], '~', char);
impl_locate_for_token!(syn::Token![_], '_', char);

impl Locate for syn::token::Group {
    fn find_loc(&self, file_path: &'static str, _code: &str, offset: usize) -> Location {
        Location {
            file_path,
            start: offset,
            end: offset,
        }
    }
}

macro_rules! impl_locate_for_pair_tokens {
    ($ty:ty, $open:literal, $close:literal) => {
        impl Locate for $ty {
            fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
                const OPEN: char = $open;
                const CLOSE: char = $close;

                let code = &code[offset..];

                let mut start = 0;
                let mut end = 0;
                let mut cur = offset;
                let mut level = 0;

                for c in code.chars() {
                    if c == OPEN {
                        if level == 0 {
                            start = cur;
                        }
                        level += 1;
                    } else if c == CLOSE {
                        if level == 1 {
                            end = cur + CLOSE.len_utf8();
                            break;
                        }
                        if level > 0 {
                            level -= 1;
                        }
                    }
                    cur += c.len_utf8();
                }

                if start >= end {
                    panic!("expected `{OPEN}..{CLOSE}` from {code}");
                }

                Location {
                    file_path,
                    start,
                    end,
                }
            }
        }
    };
}

impl_locate_for_pair_tokens!(syn::token::Brace, '{', '}');
impl_locate_for_pair_tokens!(syn::token::Bracket, '[', ']');
impl_locate_for_pair_tokens!(syn::token::Paren, '(', ')');

impl Locate for syn::Abi {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.extern_token, &self.name).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::AngleBracketedGenericArguments {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.colon2_token,
            &self.lt_token,
            &self.args,
            &self.gt_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Arm {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((if_token, guard)) = &self.guard {
            (
                &self.attrs,
                &self.pat,
                if_token,
                guard,
                &self.fat_arrow_token,
                &self.body,
                &self.comma,
            )
                .locate_as_group(file_path, code, offset)
        } else {
            (
                &self.attrs,
                &self.pat,
                &self.fat_arrow_token,
                &self.body,
                &self.comma,
            )
                .locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::AssocConst {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.ident, &self.generics, &self.eq_token, &self.value)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::AssocType {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.ident, &self.generics, &self.eq_token, &self.ty)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Attribute {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let pound_loc = self.pound_token.locate(file_path, code, offset);
        let bracket_loc = self.bracket_token.locate(file_path, code, pound_loc.end);
        self.meta.locate(file_path, code, bracket_loc.start + 1);

        Location {
            file_path,
            start: pound_loc.start,
            end: bracket_loc.end,
        }
    }
}

impl Locate for syn::BareFnArg {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((name, colon_token)) = &self.name {
            (&self.attrs, name, colon_token, &self.ty).locate_as_group(file_path, code, offset)
        } else {
            (&self.attrs, &self.ty).locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::BareVariadic {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((name, colon_token)) = &self.name {
            (&self.attrs, name, colon_token, &self.dots, &self.comma)
                .locate_as_group(file_path, code, offset)
        } else {
            (&self.attrs, &self.dots, &self.comma).locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::BinOp {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Add(v) => v.locate(file_path, code, offset),
            Self::Sub(v) => v.locate(file_path, code, offset),
            Self::Mul(v) => v.locate(file_path, code, offset),
            Self::Div(v) => v.locate(file_path, code, offset),
            Self::Rem(v) => v.locate(file_path, code, offset),
            Self::And(v) => v.locate(file_path, code, offset),
            Self::Or(v) => v.locate(file_path, code, offset),
            Self::BitXor(v) => v.locate(file_path, code, offset),
            Self::BitAnd(v) => v.locate(file_path, code, offset),
            Self::BitOr(v) => v.locate(file_path, code, offset),
            Self::Shl(v) => v.locate(file_path, code, offset),
            Self::Shr(v) => v.locate(file_path, code, offset),
            Self::Eq(v) => v.locate(file_path, code, offset),
            Self::Lt(v) => v.locate(file_path, code, offset),
            Self::Le(v) => v.locate(file_path, code, offset),
            Self::Ne(v) => v.locate(file_path, code, offset),
            Self::Ge(v) => v.locate(file_path, code, offset),
            Self::Gt(v) => v.locate(file_path, code, offset),
            Self::AddAssign(v) => v.locate(file_path, code, offset),
            Self::SubAssign(v) => v.locate(file_path, code, offset),
            Self::MulAssign(v) => v.locate(file_path, code, offset),
            Self::DivAssign(v) => v.locate(file_path, code, offset),
            Self::RemAssign(v) => v.locate(file_path, code, offset),
            Self::BitXorAssign(v) => v.locate(file_path, code, offset),
            Self::BitAndAssign(v) => v.locate(file_path, code, offset),
            Self::BitOrAssign(v) => v.locate(file_path, code, offset),
            Self::ShlAssign(v) => v.locate(file_path, code, offset),
            Self::ShrAssign(v) => v.locate(file_path, code, offset),
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::Block {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.brace_token,
            inner: &self.stmts,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::BoundLifetimes {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.for_token,
            &self.lt_token,
            &self.lifetimes,
            &self.gt_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::CapturedParam {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Lifetime(v) => v.locate(file_path, code, offset),
            Self::Ident(v) => v.locate(file_path, code, offset),
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::ConstParam {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.const_token,
            &self.ident,
            &self.colon_token,
            &self.ty,
            &self.eq_token,
            &self.default,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Constraint {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.ident, &self.generics, &self.colon_token, &self.bounds)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Expr {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Array(v) => v.locate(file_path, code, offset),
            Self::Assign(v) => v.locate(file_path, code, offset),
            Self::Async(v) => v.locate(file_path, code, offset),
            Self::Await(v) => v.locate(file_path, code, offset),
            Self::Binary(v) => v.locate(file_path, code, offset),
            Self::Block(v) => v.locate(file_path, code, offset),
            Self::Break(v) => v.locate(file_path, code, offset),
            Self::Call(v) => v.locate(file_path, code, offset),
            Self::Cast(v) => v.locate(file_path, code, offset),
            Self::Closure(v) => v.locate(file_path, code, offset),
            Self::Const(v) => v.locate(file_path, code, offset),
            Self::Continue(v) => v.locate(file_path, code, offset),
            Self::Field(v) => v.locate(file_path, code, offset),
            Self::ForLoop(v) => v.locate(file_path, code, offset),
            Self::Group(v) => v.locate(file_path, code, offset),
            Self::If(v) => v.locate(file_path, code, offset),
            Self::Index(v) => v.locate(file_path, code, offset),
            Self::Infer(v) => v.locate(file_path, code, offset),
            Self::Let(v) => v.locate(file_path, code, offset),
            Self::Lit(v) => v.locate(file_path, code, offset),
            Self::Loop(v) => v.locate(file_path, code, offset),
            Self::Macro(v) => v.locate(file_path, code, offset),
            Self::Match(v) => v.locate(file_path, code, offset),
            Self::MethodCall(v) => v.locate(file_path, code, offset),
            Self::Paren(v) => v.locate(file_path, code, offset),
            Self::Path(v) => v.locate(file_path, code, offset),
            Self::Range(v) => v.locate(file_path, code, offset),
            Self::RawAddr(v) => v.locate(file_path, code, offset),
            Self::Reference(v) => v.locate(file_path, code, offset),
            Self::Repeat(v) => v.locate(file_path, code, offset),
            Self::Return(v) => v.locate(file_path, code, offset),
            Self::Struct(v) => v.locate(file_path, code, offset),
            Self::Try(v) => v.locate(file_path, code, offset),
            Self::TryBlock(v) => v.locate(file_path, code, offset),
            Self::Tuple(v) => v.locate(file_path, code, offset),
            Self::Unary(v) => v.locate(file_path, code, offset),
            Self::Unsafe(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            Self::While(v) => v.locate(file_path, code, offset),
            Self::Yield(v) => v.locate(file_path, code, offset),
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::ExprArray {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: &self.attrs,
            surround: &self.bracket_token,
            inner: &self.elems,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ExprAssign {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.left, &self.eq_token, &self.right)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprAsync {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.async_token, &self.capture, &self.block)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprAwait {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.base, &self.dot_token, &self.await_token)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprBinary {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.left, &self.op, &self.right).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprBlock {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.label, &self.block).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprBreak {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.break_token, &self.label, &self.expr)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprCall {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (&self.attrs, &self.func),
            surround: &self.paren_token,
            inner: &self.args,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ExprCast {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.expr, &self.as_token, &self.ty).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprClosure {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.lifetimes,
            &self.constness,
            &self.movability,
            &self.asyncness,
            &self.capture,
            &self.or1_token,
            &self.inputs,
            &self.or2_token,
            &self.output,
            &self.body,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprConst {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.const_token, &self.block).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprContinue {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.continue_token, &self.label).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprField {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.base, &self.dot_token, &self.member)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprForLoop {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.label,
            &self.for_token,
            &self.pat,
            &self.in_token,
            &self.expr,
            &self.body,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprGroup {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.group_token, &self.expr).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprIf {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((else_token, else_branch)) = &self.else_branch {
            (
                &self.attrs,
                &self.if_token,
                &self.cond,
                &self.then_branch,
                else_token,
                else_branch,
            )
                .locate_as_group(file_path, code, offset)
        } else {
            (&self.attrs, &self.if_token, &self.cond, &self.then_branch)
                .locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::ExprIndex {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (&self.attrs, &self.expr),
            surround: &self.bracket_token,
            inner: &self.index,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ExprInfer {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.underscore_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprLet {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.let_token,
            &self.pat,
            &self.eq_token,
            &self.expr,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprLit {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.lit).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprLoop {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.label, &self.loop_token, &self.body)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprMacro {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.mac).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprMatch {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (&self.attrs, &self.match_token, &self.expr),
            surround: &self.brace_token,
            inner: &self.arms,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ExprMethodCall {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (
                &self.attrs,
                &self.receiver,
                &self.dot_token,
                &self.method,
                &self.turbofish,
            ),
            surround: &self.paren_token,
            inner: &self.args,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ExprParen {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: &self.attrs,
            surround: &self.paren_token,
            inner: &self.expr,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ExprPath {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some(qself) = &self.qself {
            Qualified {
                front: &self.attrs,
                qself,
                path: &self.path,
                back: (),
            }
            .locate(file_path, code, offset)
        } else {
            (&self.attrs, &self.path).locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::ExprRange {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match (&self.start, &self.end) {
            (Some(start), Some(end)) => {
                (&self.attrs, start, &self.limits, end).locate_as_group(file_path, code, offset)
            }
            (Some(start), None) => {
                (&self.attrs, start, &self.limits).locate_as_group(file_path, code, offset)
            }
            (None, Some(end)) => {
                (&self.attrs, &self.limits, end).locate_as_group(file_path, code, offset)
            }
            (None, None) => (&self.attrs, &self.limits).locate_as_group(file_path, code, offset),
        }
    }
}

impl Locate for syn::ExprRawAddr {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.and_token,
            &self.raw,
            &self.mutability,
            &self.expr,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprReference {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.and_token, &self.mutability, &self.expr)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprRepeat {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: &self.attrs,
            surround: &self.bracket_token,
            inner: (&self.expr, &self.semi_token, &self.len),
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ExprReturn {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.return_token, &self.expr).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprStruct {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let front_loc = if let Some(qself) = &self.qself {
            Qualified {
                front: &self.attrs,
                qself,
                path: &self.path,
                back: (),
            }
            .locate(file_path, code, offset)
        } else {
            (&self.attrs, &self.path).locate_as_group(file_path, code, offset)
        };

        let back_loc = Surround {
            front: (),
            surround: &self.brace_token,
            inner: (&self.fields, &self.dot2_token, &self.rest),
            back: (),
        }
        .locate(file_path, code, front_loc.end);

        Location {
            file_path,
            start: front_loc.start,
            end: back_loc.end,
        }
    }
}

impl Locate for syn::ExprTry {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.expr, &self.question_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprTryBlock {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.try_token, &self.block).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprTuple {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: &self.attrs,
            surround: &self.paren_token,
            inner: &self.elems,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ExprUnary {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.op, &self.expr).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprUnsafe {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.unsafe_token, &self.block).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprWhile {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.label,
            &self.while_token,
            &self.cond,
            &self.body,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ExprYield {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.yield_token, &self.expr).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Field {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.mutability,
            &self.ident,
            &self.colon_token,
            &self.ty,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::FieldMutability {
    fn find_loc(&self, file_path: &'static str, _code: &str, offset: usize) -> Location {
        Location {
            file_path,
            start: offset,
            end: offset,
        }
    }
}

impl Locate for syn::FieldPat {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.member, &self.colon_token, &self.pat)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Fields {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Named(v) => v.locate(file_path, code, offset),
            Self::Unnamed(v) => v.locate(file_path, code, offset),
            Self::Unit => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::FieldsNamed {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.brace_token,
            inner: &self.named,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::FieldsUnnamed {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.paren_token,
            inner: &self.unnamed,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::FieldValue {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.member, &self.colon_token, &self.expr)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::File {
    fn find_loc(&self, file_path: &'static str, code: &str, _offset: usize) -> Location {
        (&self.attrs, &self.items).locate_as_group(file_path, code, 0)
    }
}

impl Locate for syn::FnArg {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Receiver(v) => v.locate(file_path, code, offset),
            Self::Typed(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::ForeignItem {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Fn(v) => v.locate(file_path, code, offset),
            Self::Static(v) => v.locate(file_path, code, offset),
            Self::Type(v) => v.locate(file_path, code, offset),
            Self::Macro(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::ForeignItemFn {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.vis, &self.sig, &self.semi_token)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ForeignItemStatic {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.static_token,
            &self.mutability,
            &self.ident,
            &self.colon_token,
            &self.ty,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ForeignItemType {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.type_token,
            &self.ident,
            &self.generics,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ForeignItemMacro {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.mac, &self.semi_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::GenericArgument {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Lifetime(v) => v.locate(file_path, code, offset),
            Self::Type(v) => v.locate(file_path, code, offset),
            Self::Const(v) => v.locate(file_path, code, offset),
            Self::AssocType(v) => v.locate(file_path, code, offset),
            Self::AssocConst(v) => v.locate(file_path, code, offset),
            Self::Constraint(v) => v.locate(file_path, code, offset),
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::GenericParam {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Lifetime(v) => v.locate(file_path, code, offset),
            Self::Type(v) => v.locate(file_path, code, offset),
            Self::Const(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::Generics {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.lt_token,
            &self.params,
            &self.gt_token,
            &self.where_clause,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Ident {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let code = &code[offset..];

        let ident = self.to_string();
        let start = offset
            + code
                .find(&ident)
                .unwrap_or_else(|| panic!("expected `{ident}` from `{code}`"));

        Location {
            file_path,
            start,
            end: start + ident.len(),
        }
    }
}

impl Locate for syn::ImplItem {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Const(v) => v.locate(file_path, code, offset),
            Self::Fn(v) => v.locate(file_path, code, offset),
            Self::Type(v) => v.locate(file_path, code, offset),
            Self::Macro(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::ImplItemConst {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.defaultness,
            &self.const_token,
            &self.ident,
            &self.generics,
            &self.colon_token,
            &self.ty,
            &self.eq_token,
            &self.expr,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ImplItemFn {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.defaultness,
            &self.sig,
            &self.block,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ImplItemType {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.defaultness,
            &self.type_token,
            &self.ident,
            &self.generics,
            &self.eq_token,
            &self.ty,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ImplItemMacro {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.mac, &self.semi_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ImplRestriction {
    fn find_loc(&self, file_path: &'static str, _code: &str, offset: usize) -> Location {
        Location {
            file_path,
            start: offset,
            end: offset,
        }
    }
}

impl Locate for syn::Index {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let value = self.index.to_string();
        helper::str_location(file_path, code, offset, &value)
    }
}

impl Locate for syn::Item {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Const(v) => v.locate(file_path, code, offset),
            Self::Enum(v) => v.locate(file_path, code, offset),
            Self::ExternCrate(v) => v.locate(file_path, code, offset),
            Self::Fn(v) => v.locate(file_path, code, offset),
            Self::ForeignMod(v) => v.locate(file_path, code, offset),
            Self::Impl(v) => v.locate(file_path, code, offset),
            Self::Macro(v) => v.locate(file_path, code, offset),
            Self::Mod(v) => v.locate(file_path, code, offset),
            Self::Static(v) => v.locate(file_path, code, offset),
            Self::Struct(v) => v.locate(file_path, code, offset),
            Self::Trait(v) => v.locate(file_path, code, offset),
            Self::TraitAlias(v) => v.locate(file_path, code, offset),
            Self::Type(v) => v.locate(file_path, code, offset),
            Self::Union(v) => v.locate(file_path, code, offset),
            Self::Use(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::ItemConst {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.const_token,
            &self.ident,
            &self.generics,
            &self.colon_token,
            &self.ty,
            &self.eq_token,
            &self.expr,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ItemEnum {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (
                &self.attrs,
                &self.vis,
                &self.enum_token,
                &self.ident,
                &self.generics,
            ),
            surround: &self.brace_token,
            inner: &self.variants,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ItemExternCrate {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((as_token, rename)) = &self.rename {
            (
                &self.attrs,
                &self.vis,
                &self.extern_token,
                &self.crate_token,
                &self.ident,
                as_token,
                rename,
                &self.semi_token,
            )
                .locate_as_group(file_path, code, offset)
        } else {
            (
                &self.attrs,
                &self.vis,
                &self.extern_token,
                &self.crate_token,
                &self.ident,
                &self.semi_token,
            )
                .locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::ItemFn {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.vis, &self.sig, &self.block).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ItemForeignMod {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (&self.attrs, &self.unsafety, &self.abi),
            surround: &self.brace_token,
            inner: &self.items,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ItemImpl {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((exc_token, path, for_token)) = &self.trait_ {
            Surround {
                front: (
                    &self.attrs,
                    &self.defaultness,
                    &self.unsafety,
                    &self.impl_token,
                    &self.generics,
                    exc_token,
                    path,
                    for_token,
                    &self.self_ty,
                ),
                surround: &self.brace_token,
                inner: &self.items,
                back: (),
            }
            .locate(file_path, code, offset)
        } else {
            Surround {
                front: (
                    &self.attrs,
                    &self.defaultness,
                    &self.unsafety,
                    &self.impl_token,
                    &self.generics,
                    &self.self_ty,
                ),
                surround: &self.brace_token,
                inner: &self.items,
                back: (),
            }
            .locate(file_path, code, offset)
        }
    }
}

impl Locate for syn::ItemMacro {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.ident, &self.mac, &self.semi_token)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ItemMod {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match (&self.content, &self.semi) {
            (Some((brace, items)), Some(semi_token)) => Surround {
                front: (&self.attrs, &self.vis, &self.mod_token, &self.ident),
                surround: brace,
                inner: items,
                back: semi_token,
            }
            .locate(file_path, code, offset),
            (Some((brace, items)), None) => Surround {
                front: (&self.attrs, &self.vis, &self.mod_token, &self.ident),
                surround: brace,
                inner: items,
                back: (),
            }
            .locate(file_path, code, offset),
            (None, Some(semi_token)) => (
                &self.attrs,
                &self.vis,
                &self.mod_token,
                &self.ident,
                semi_token,
            )
                .locate_as_group(file_path, code, offset),
            (None, None) => (&self.attrs, &self.vis, &self.mod_token, &self.ident)
                .locate_as_group(file_path, code, offset),
        }
    }
}

impl Locate for syn::ItemStatic {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.static_token,
            &self.mutability,
            &self.ident,
            &self.colon_token,
            &self.ty,
            &self.eq_token,
            &self.expr,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ItemStruct {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.struct_token,
            &self.ident,
            &self.generics,
            &self.fields,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ItemTrait {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (
                &self.attrs,
                &self.vis,
                &self.unsafety,
                &self.auto_token,
                &self.restriction,
                &self.trait_token,
                &self.ident,
                &self.generics,
                &self.colon_token,
                &self.supertraits,
            ),
            surround: &self.brace_token,
            inner: &self.items,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::ItemTraitAlias {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.trait_token,
            &self.ident,
            &self.generics,
            &self.eq_token,
            &self.bounds,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ItemType {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.type_token,
            &self.ident,
            &self.generics,
            &self.eq_token,
            &self.ty,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ItemUnion {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.union_token,
            &self.ident,
            &self.generics,
            &self.fields,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ItemUse {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.vis,
            &self.use_token,
            &self.leading_colon,
            &self.tree,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Label {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.name, &self.colon_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Lifetime {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let code = &code[offset..];

        let start = offset
            + code
                .find('\'')
                .unwrap_or_else(|| panic!("expected ' from {code}"));
        let end = self.ident.locate(file_path, code, start + 1).end;

        Location {
            file_path,
            start,
            end,
        }
    }
}

impl Locate for syn::LifetimeParam {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.lifetime, &self.colon_token, &self.bounds)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Lit {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Str(v) => v.locate(file_path, code, offset),
            Self::ByteStr(v) => v.locate(file_path, code, offset),
            Self::CStr(v) => v.locate(file_path, code, offset),
            Self::Byte(v) => v.locate(file_path, code, offset),
            Self::Char(v) => v.locate(file_path, code, offset),
            Self::Int(v) => v.locate(file_path, code, offset),
            Self::Float(v) => v.locate(file_path, code, offset),
            Self::Bool(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::LitStr {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let lit = self.token().to_string();
        helper::str_location(file_path, code, offset, &lit)
    }
}

impl Locate for syn::LitByteStr {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let lit = self.token().to_string();
        helper::str_location(file_path, code, offset, &lit)
    }
}

impl Locate for syn::LitCStr {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let lit = self.token().to_string();
        helper::str_location(file_path, code, offset, &lit)
    }
}

impl Locate for syn::LitByte {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let lit = self.token().to_string();
        helper::str_location(file_path, code, offset, &lit)
    }
}

impl Locate for syn::LitChar {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let lit = self.token().to_string();
        helper::str_location(file_path, code, offset, &lit)
    }
}

impl Locate for syn::LitInt {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let lit = self.token().to_string();
        helper::str_location(file_path, code, offset, &lit)
    }
}

impl Locate for syn::LitFloat {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let lit = self.token().to_string();
        helper::str_location(file_path, code, offset, &lit)
    }
}

impl Locate for syn::LitBool {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let lit = self.token().to_string();
        helper::str_location(file_path, code, offset, &lit)
    }
}

impl Locate for syn::Local {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.let_token,
            &self.pat,
            &self.init,
            &self.semi_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::LocalInit {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((else_token, diverge)) = &self.diverge {
            (&self.eq_token, &self.expr, else_token, diverge)
                .locate_as_group(file_path, code, offset)
        } else {
            (&self.eq_token, &self.expr).locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::Macro {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match &self.delimiter {
            syn::MacroDelimiter::Paren(paren) => Surround {
                front: (&self.path, &self.bang_token),
                surround: paren,
                inner: (),
                back: (),
            }
            .locate(file_path, code, offset),
            syn::MacroDelimiter::Brace(brace) => Surround {
                front: (&self.path, &self.bang_token),
                surround: brace,
                inner: (),
                back: (),
            }
            .locate(file_path, code, offset),
            syn::MacroDelimiter::Bracket(bracket) => Surround {
                front: (&self.path, &self.bang_token),
                surround: bracket,
                inner: (),
                back: (),
            }
            .locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::Member {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Named(v) => v.locate(file_path, code, offset),
            Self::Unnamed(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::Meta {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Path(v) => v.locate(file_path, code, offset),
            Self::List(v) => v.locate(file_path, code, offset),
            Self::NameValue(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::MetaList {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match &self.delimiter {
            syn::MacroDelimiter::Paren(paren) => Surround {
                front: &self.path,
                surround: paren,
                inner: (),
                back: (),
            }
            .locate(file_path, code, offset),
            syn::MacroDelimiter::Brace(brace) => Surround {
                front: &self.path,
                surround: brace,
                inner: (),
                back: (),
            }
            .locate(file_path, code, offset),
            syn::MacroDelimiter::Bracket(bracket) => Surround {
                front: &self.path,
                surround: bracket,
                inner: (),
                back: (),
            }
            .locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::MetaNameValue {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.path, &self.eq_token, &self.value).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::ParenthesizedGenericArguments {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.paren_token,
            inner: &self.inputs,
            back: &self.output,
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::Pat {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Const(v) => v.locate(file_path, code, offset),
            Self::Ident(v) => v.locate(file_path, code, offset),
            Self::Lit(v) => v.locate(file_path, code, offset),
            Self::Macro(v) => v.locate(file_path, code, offset),
            Self::Or(v) => v.locate(file_path, code, offset),
            Self::Paren(v) => v.locate(file_path, code, offset),
            Self::Path(v) => v.locate(file_path, code, offset),
            Self::Range(v) => v.locate(file_path, code, offset),
            Self::Reference(v) => v.locate(file_path, code, offset),
            Self::Rest(v) => v.locate(file_path, code, offset),
            Self::Slice(v) => v.locate(file_path, code, offset),
            Self::Struct(v) => v.locate(file_path, code, offset),
            Self::Tuple(v) => v.locate(file_path, code, offset),
            Self::TupleStruct(v) => v.locate(file_path, code, offset),
            Self::Type(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            Self::Wild(v) => v.locate(file_path, code, offset),
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::PatIdent {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((at_token, subpat)) = &self.subpat {
            (
                &self.attrs,
                &self.by_ref,
                &self.mutability,
                &self.ident,
                at_token,
                subpat,
            )
                .locate_as_group(file_path, code, offset)
        } else {
            (&self.attrs, &self.by_ref, &self.mutability, &self.ident)
                .locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::PatOr {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.leading_vert, &self.cases).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::PatParen {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: &self.attrs,
            surround: &self.paren_token,
            inner: &self.pat,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::PatReference {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.and_token, &self.mutability, &self.pat)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::PatRest {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.dot2_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::PatSlice {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: &self.attrs,
            surround: &self.bracket_token,
            inner: &self.elems,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::PatStruct {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let front_loc = if let Some(qself) = &self.qself {
            Qualified {
                front: &self.attrs,
                qself,
                path: &self.path,
                back: (),
            }
            .locate(file_path, code, offset)
        } else {
            (&self.attrs, &self.path).locate_as_group(file_path, code, offset)
        };

        let back_loc = Surround {
            front: (),
            surround: &self.brace_token,
            inner: (&self.fields, &self.rest),
            back: (),
        }
        .locate(file_path, code, front_loc.end);

        Location {
            file_path,
            start: front_loc.start,
            end: back_loc.end,
        }
    }
}

impl Locate for syn::PatTuple {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: &self.attrs,
            surround: &self.paren_token,
            inner: &self.elems,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::PatTupleStruct {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let front_loc = if let Some(qself) = &self.qself {
            Qualified {
                front: &self.attrs,
                qself,
                path: &self.path,
                back: (),
            }
            .locate(file_path, code, offset)
        } else {
            (&self.attrs, &self.path).locate_as_group(file_path, code, offset)
        };

        let back_loc = Surround {
            front: (),
            surround: &self.paren_token,
            inner: &self.elems,
            back: (),
        }
        .locate(file_path, code, front_loc.end);

        Location {
            file_path,
            start: front_loc.start,
            end: back_loc.end,
        }
    }
}

impl Locate for syn::PatType {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.pat, &self.colon_token, &self.ty)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::PatWild {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.underscore_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Path {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.leading_colon, &self.segments).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::PathArguments {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::None => Location {
                file_path,
                start: offset,
                end: offset,
            },
            Self::AngleBracketed(v) => v.locate(file_path, code, offset),
            Self::Parenthesized(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::PathSegment {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.ident, &self.arguments).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::PointerMutability {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Const(v) => v.locate(file_path, code, offset),
            Self::Mut(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::PreciseCapture {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.use_token,
            &self.lt_token,
            &self.params,
            &self.gt_token,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::PredicateLifetime {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.lifetime, &self.colon_token, &self.bounds).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::PredicateType {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.lifetimes,
            &self.bounded_ty,
            &self.colon_token,
            &self.bounds,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::QSelf {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let front_loc =
            (&self.lt_token, &self.ty, &self.as_token).locate_as_group(file_path, code, offset);

        const OPEN: char = '<';
        const CLOSE: char = '>';

        let code = &code[front_loc.end..];

        let mut cur = front_loc.end;
        let mut level = 1;

        for c in code.chars() {
            if c == OPEN {
                level += 1;
            } else if c == CLOSE {
                if level == 1 {
                    break;
                }
                level -= 1;
            }
            cur += c.len_utf8();
        }

        let end = self.gt_token.locate(file_path, code, cur).end;

        Location {
            file_path,
            start: front_loc.start,
            end,
        }
    }
}

impl Locate for syn::RangeLimits {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::HalfOpen(v) => v.locate(file_path, code, offset),
            Self::Closed(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::Receiver {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((and_token, reference)) = &self.reference {
            (
                &self.attrs,
                and_token,
                reference,
                &self.mutability,
                &self.self_token,
                &self.colon_token,
                &self.ty,
            )
                .locate_as_group(file_path, code, offset)
        } else {
            (
                &self.attrs,
                &self.mutability,
                &self.self_token,
                &self.colon_token,
                &self.ty,
            )
                .locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::ReturnType {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Default => Location {
                file_path,
                start: offset,
                end: offset,
            },
            Self::Type(arrow_token, ty) => {
                (arrow_token, ty).locate_as_group(file_path, code, offset)
            }
        }
    }
}

impl Locate for syn::Signature {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (
                &self.constness,
                &self.asyncness,
                &self.unsafety,
                &self.abi,
                &self.fn_token,
                &self.ident,
                &self.generics,
            ),
            surround: &self.paren_token,
            inner: (&self.inputs, &self.variadic),
            back: &self.output,
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::StaticMutability {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Mut(v) => v.locate(file_path, code, offset),
            Self::None => Location {
                file_path,
                start: offset,
                end: offset,
            },
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::Stmt {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Local(v) => v.locate(file_path, code, offset),
            Self::Item(v) => v.locate(file_path, code, offset),
            Self::Expr(expr, semi_token) => {
                (expr, semi_token).locate_as_group(file_path, code, offset)
            }
            Self::Macro(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::StmtMacro {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.mac, &self.semi_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TraitBound {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        // self.paren_token is always Null according to syn parsing.
        (&self.modifier, &self.lifetimes, &self.path).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TraitBoundModifier {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::None => Location {
                file_path,
                start: offset,
                end: offset,
            },
            Self::Maybe(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::TraitItem {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Const(v) => v.locate(file_path, code, offset),
            Self::Fn(v) => v.locate(file_path, code, offset),
            Self::Type(v) => v.locate(file_path, code, offset),
            Self::Macro(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::TraitItemConst {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((eq_token, default)) = &self.default {
            (
                &self.attrs,
                &self.const_token,
                &self.ident,
                &self.generics,
                &self.colon_token,
                &self.ty,
                eq_token,
                default,
                &self.semi_token,
            )
                .locate_as_group(file_path, code, offset)
        } else {
            (
                &self.attrs,
                &self.const_token,
                &self.ident,
                &self.generics,
                &self.colon_token,
                &self.ty,
                &self.semi_token,
            )
                .locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::TraitItemFn {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.sig, &self.default, &self.semi_token)
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TraitItemType {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((eq_token, default)) = &self.default {
            (
                &self.attrs,
                &self.type_token,
                &self.ident,
                &self.generics,
                &self.colon_token,
                &self.bounds,
                eq_token,
                default,
                &self.semi_token,
            )
                .locate_as_group(file_path, code, offset)
        } else {
            (
                &self.attrs,
                &self.type_token,
                &self.ident,
                &self.generics,
                &self.colon_token,
                &self.bounds,
                &self.semi_token,
            )
                .locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::TraitItemMacro {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.attrs, &self.mac, &self.semi_token).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::Type {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Array(v) => v.locate(file_path, code, offset),
            Self::BareFn(v) => v.locate(file_path, code, offset),
            Self::Group(v) => v.locate(file_path, code, offset),
            Self::ImplTrait(v) => v.locate(file_path, code, offset),
            Self::Infer(v) => v.locate(file_path, code, offset),
            Self::Macro(v) => v.locate(file_path, code, offset),
            Self::Never(v) => v.locate(file_path, code, offset),
            Self::Paren(v) => v.locate(file_path, code, offset),
            Self::Path(v) => v.locate(file_path, code, offset),
            Self::Ptr(v) => v.locate(file_path, code, offset),
            Self::Reference(v) => v.locate(file_path, code, offset),
            Self::Slice(v) => v.locate(file_path, code, offset),
            Self::TraitObject(v) => v.locate(file_path, code, offset),
            Self::Tuple(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::TypeArray {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.bracket_token,
            inner: (&self.elem, &self.semi_token, &self.len),
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::TypeBareFn {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (&self.lifetimes, &self.unsafety, &self.abi, &self.fn_token),
            surround: &self.paren_token,
            inner: (&self.inputs, &self.variadic),
            back: &self.output,
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::TypeGroup {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.group_token, &self.elem).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TypeImplTrait {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.impl_token, &self.bounds).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TypeInfer {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        self.underscore_token.locate(file_path, code, offset)
    }
}

impl Locate for syn::TypeMacro {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        self.mac.locate(file_path, code, offset)
    }
}

impl Locate for syn::TypeNever {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        self.bang_token.locate(file_path, code, offset)
    }
}

impl Locate for syn::TypeParen {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.paren_token,
            inner: &self.elem,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::TypePath {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some(qself) = &self.qself {
            Qualified {
                front: (),
                qself,
                path: &self.path,
                back: (),
            }
            .locate(file_path, code, offset)
        } else {
            self.path.locate(file_path, code, offset)
        }
    }
}

impl Locate for syn::TypePtr {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.star_token,
            &self.const_token,
            &self.mutability,
            &self.elem,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TypeReference {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.and_token,
            &self.lifetime,
            &self.mutability,
            &self.elem,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TypeSlice {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.bracket_token,
            inner: &self.elem,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::TypeTraitObject {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.dyn_token, &self.bounds).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TypeTuple {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.paren_token,
            inner: &self.elems,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::TypeParam {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (
            &self.attrs,
            &self.ident,
            &self.colon_token,
            &self.bounds,
            &self.eq_token,
            &self.default,
        )
            .locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::TypeParamBound {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Trait(v) => v.locate(file_path, code, offset),
            Self::Lifetime(v) => v.locate(file_path, code, offset),
            Self::PreciseCapture(v) => v.locate(file_path, code, offset),
            Self::Verbatim(_) => Location {
                file_path,
                start: offset,
                end: offset,
            },
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::UnOp {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Deref(v) => v.locate(file_path, code, offset),
            Self::Not(v) => v.locate(file_path, code, offset),
            Self::Neg(v) => v.locate(file_path, code, offset),
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::UseGlob {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        self.star_token.locate(file_path, code, offset)
    }
}

impl Locate for syn::UseGroup {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: (),
            surround: &self.brace_token,
            inner: &self.items,
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::UseName {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        self.ident.locate(file_path, code, offset)
    }
}

impl Locate for syn::UsePath {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.ident, &self.colon2_token, &self.tree).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::UseRename {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.ident, &self.as_token, &self.rename).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::UseTree {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Path(v) => v.locate(file_path, code, offset),
            Self::Name(v) => v.locate(file_path, code, offset),
            Self::Rename(v) => v.locate(file_path, code, offset),
            Self::Glob(v) => v.locate(file_path, code, offset),
            Self::Group(v) => v.locate(file_path, code, offset),
        }
    }
}

impl Locate for syn::Variadic {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((pat, colon_token)) = &self.pat {
            (&self.attrs, pat, colon_token, &self.dots, &self.comma)
                .locate_as_group(file_path, code, offset)
        } else {
            (&self.attrs, &self.dots, &self.comma).locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::Variant {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some((eq_token, discriminant)) = &self.discriminant {
            (
                &self.attrs,
                &self.ident,
                &self.fields,
                eq_token,
                discriminant,
            )
                .locate_as_group(file_path, code, offset)
        } else {
            (&self.attrs, &self.ident, &self.fields).locate_as_group(file_path, code, offset)
        }
    }
}

impl Locate for syn::Visibility {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Public(v) => v.locate(file_path, code, offset),
            Self::Restricted(v) => v.locate(file_path, code, offset),
            Self::Inherited => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

impl Locate for syn::VisRestricted {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        Surround {
            front: &self.pub_token,
            surround: &self.paren_token,
            inner: (&self.in_token, &self.path),
            back: (),
        }
        .locate(file_path, code, offset)
    }
}

impl Locate for syn::WhereClause {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        (&self.where_token, &self.predicates).locate_as_group(file_path, code, offset)
    }
}

impl Locate for syn::WherePredicate {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        match self {
            Self::Lifetime(v) => v.locate(file_path, code, offset),
            Self::Type(v) => v.locate(file_path, code, offset),
            _ => Location {
                file_path,
                start: offset,
                end: offset,
            },
        }
    }
}

// === Composite types ===

impl<T: Locate> Locate for Option<T> {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        if let Some(inner) = self {
            inner.locate(file_path, code, offset)
        } else {
            Location {
                file_path,
                start: offset,
                end: offset,
            }
        }
    }
}

impl<T: Locate> Locate for Box<T> {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let t = &**self;
        t.locate(file_path, code, offset)
    }
}

impl<T: Locate> Locate for Vec<T> {
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let mut start = usize::MAX;
        let mut end = offset;

        for item in self {
            let loc = item.locate(file_path, code, end);
            start = start.min(loc.start);
            end = loc.end;
        }

        Location {
            file_path,
            start: if start != usize::MAX { start } else { offset },
            end,
        }
    }
}

impl<T, S> Locate for syn::punctuated::Punctuated<T, S>
where
    T: Locate,
    S: Locate,
{
    fn find_loc(&self, file_path: &'static str, code: &str, offset: usize) -> Location {
        let mut start = usize::MAX;
        let mut end = offset;

        for item in self {
            let loc = item.locate(file_path, code, end);
            start = start.min(loc.start);
            end = loc.end;
        }

        Location {
            file_path,
            start: if start != usize::MAX { start } else { offset },
            end,
        }
    }
}

// === Helper functions ===

pub mod helper {
    use super::*;

    pub fn char_location(
        file_path: &'static str,
        code: &str,
        offset: usize,
        content: char,
    ) -> Location {
        let code = &code[offset..];
        let start = offset
            + code
                .find(content)
                .unwrap_or_else(|| panic!("expected `{content}` from `{code}`"));

        Location {
            file_path,
            start,
            end: start + content.len_utf8(),
        }
    }

    pub fn str_location(
        file_path: &'static str,
        code: &str,
        offset: usize,
        content: &str,
    ) -> Location {
        let code = &code[offset..];

        let start = offset
            + code
                .find(content)
                .unwrap_or_else(|| panic!("expected `{content}` from `{code}`"));

        Location {
            file_path,
            start,
            end: start + content.len(),
        }
    }
}
