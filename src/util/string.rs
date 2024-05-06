use std::{borrow::Cow, rc::Rc};

pub fn concat_string(l: &str, r: &str) -> String {
    let mut s = String::with_capacity(l.len() + r.len());
    s.push_str(l);
    s.push_str(r);
    s
}

pub fn concat_opt_string(l: Option<&str>, r: &str) -> Option<String> {
    l.map(|l| concat_string(l, r))
}

/// Determines number of digits at the end of the string.
///
/// # Examples
///
/// ```
/// # use acttey::util::string::rdigit_num;
///
/// assert_eq!(2, rdigit_num("hello42"));
/// assert_eq!(0, rdigit_num("1a")); // String doesn't end with digits.
/// ```
pub fn rdigit_num(s: &str) -> usize {
    let v = s.as_bytes();
    v.iter().rev().take_while(|c| c.is_ascii_digit()).count()
}

/// Removes ascii digit characters at the end of the string.
///
/// # Examples
///
/// ```
/// # use acttey::util::string::trim_rdigits;
///
/// let mut s = "hello42".to_owned();
/// trim_rdigits(&mut s);
/// assert_eq!("hello", s.as_str());
/// ```
pub fn trim_rdigits(s: &mut String) {
    s.truncate(s.len() - rdigit_num(s));
}

/// Increases ascii number at the end of the string.
///
/// # Examples
///
/// ```
/// # use acttey::util::string::increase_rnumber;
///
/// let mut s = "hello01".to_owned();
/// increase_rnumber(&mut s);
/// assert_eq!("hello02", s.as_str());
///
/// let mut s = "hello99".to_owned();
/// increase_rnumber(&mut s);
/// assert_eq!("hello100", s.as_str());
///
/// let mut s = "hello".to_owned();
/// increase_rnumber(&mut s);
/// assert_eq!("hello", s.as_str()); // String doesn't end with number.
/// ```
pub fn increase_rnumber(s: &mut String) {
    let n = rdigit_num(s);
    if n == 0 {
        return;
    }

    let mut carry = 1;
    // Safety: In UTF-8, byte starts with 0 is same with the ascii character.
    unsafe {
        let v = s.as_bytes_mut();
        for c in v.iter_mut().rev().take(n) {
            if carry == 0 {
                return;
            }
            let d = *c - b'0' + carry;
            carry = d / 10;
            *c = d % 10 + b'0';
        }
        if carry > 0 {
            s.insert(s.len() - n, (carry + b'0') as char);
        }
    }
}

/// Used to be shown as a string even if it's not a string type.
pub trait ToStr {
    fn to_str(&self) -> Cow<str>;
}

/// Common [`ToStr`] implementation for [`str`].
impl ToStr for str {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(self)
    }
}

/// Common [`ToStr`] implementation for [`String`].
impl ToStr for String {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(self.as_str())
    }
}

/// Common [`ToStr`] implementation for [`Rc<str>`].
impl ToStr for Rc<str> {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(self)
    }
}

/// Common [`ToStr`] implementation for [`Box<str>`].
impl ToStr for Box<str> {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(self)
    }
}

/// Encodes a single byte into base64.
#[inline(always)]
pub const fn encode_base64(byte: u8) -> u8 {
    match byte {
        0..=25 => b'A' + byte,
        26..=51 => b'a' + byte - 26,
        52..=61 => b'0' + byte - 52,
        // URL safe version, standard: '+'
        62 => b'-',
        // URL safe version, standard: '/'
        63 => b'_',
        _ => panic!(),
    }
}

/// Encodes a single u32 value into base64.
#[inline]
pub const fn encode_base64_u32(value: u32) -> [u8; 6] {
    const MASK: u32 = (1 << 6) - 1;
    [
        encode_base64(((value >> 26) & MASK) as u8),
        encode_base64(((value >> 20) & MASK) as u8),
        encode_base64(((value >> 14) & MASK) as u8),
        encode_base64(((value >> 8) & MASK) as u8),
        encode_base64(((value >> 2) & MASK) as u8),
        encode_base64(((value << 4) & MASK) as u8),
    ]
}
