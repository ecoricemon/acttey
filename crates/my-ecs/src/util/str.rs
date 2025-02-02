//! String utilities.

/// Determines number of digits at the end of the string.
///
/// # Examples
///
/// ```
/// use my_ecs::prelude::*;
///
/// assert_eq!(2, str_util::rdigit_num("hello42"));
/// assert_eq!(0, str_util::rdigit_num("1a")); // Doesn't end with digits.
/// ```
pub fn rdigit_num(s: &str) -> usize {
    let v = s.as_bytes();
    v.iter().rev().take_while(|c| c.is_ascii_digit()).count()
}

/// Increases ascii number at the end of the string.
///
/// # Examples
///
/// ```
/// use my_ecs::prelude::*;
///
/// let mut s = "hello01".to_owned();
/// str_util::increase_rnumber(&mut s);
/// assert_eq!("hello02", s.as_str());
///
/// let mut s = "hello99".to_owned();
/// str_util::increase_rnumber(&mut s);
/// assert_eq!("hello100", s.as_str());
///
/// let mut s = "hello".to_owned();
/// str_util::increase_rnumber(&mut s);
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
