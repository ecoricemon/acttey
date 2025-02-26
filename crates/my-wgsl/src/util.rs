use super::to_code::{ConstructPrettyCode, ConstructWgslCode};

pub(crate) fn str_into_xyz(s: &str) -> Result<[u32; 3], String> {
    let mut xyz = [0, 1, 1];
    let mut i = 0;
    for piece in s.split(",") {
        if let Ok(n) = piece.trim().parse::<u32>() {
            if i < 3 {
                xyz[i] = n;
                i += 1;
            } else {
                return Err(format!("cannot to convert '{s}' into (x, y, z): too long"));
            }
        }
    }

    if xyz[0] == 0 {
        return Err(format!(
            "cannot to convert '{s}' into (x, y, z): looks empty"
        ));
    }

    Ok(xyz)
}

pub(crate) fn find_index<I, II, T, F>(iter: I, target: T, mut map: F) -> Option<usize>
where
    I: Iterator<Item = II>,
    T: PartialEq,
    F: FnMut(II) -> Option<T>,
{
    iter.enumerate().find_map(|(i, ii)| {
        if let Some(ii) = map(ii) {
            (ii == target).then_some(i)
        } else {
            None
        }
    })
}

pub(crate) fn _put_str_join<'a, I, II, F>(
    iter: I,
    buf: &mut String,
    pre: &str,
    sep: &str,
    last_punct: &str,
    mut put_fn: F,
) where
    I: Iterator<Item = &'a II>,
    II: 'a,
    F: FnMut(&II, &mut String),
{
    let prev = buf.len();
    for item in iter {
        buf.push_str(pre);
        put_fn(item, buf);
        buf.push_str(sep);
    }
    if buf.len() > prev {
        for _ in 0..sep.len() {
            buf.pop();
        }
        buf.push_str(last_punct);
    }
}

pub(crate) fn put_str_join<'a, I, II>(
    iter: I,
    buf: &mut String,
    pre: &str,
    sep: &str,
    last_punct: &str,
) where
    I: Iterator<Item = &'a II>,
    II: ConstructWgslCode + 'a,
{
    _put_str_join(
        iter,
        buf,
        pre,
        sep,
        last_punct,
        ConstructWgslCode::write_wgsl_string,
    );
}

pub(crate) fn put_str_pretty_join<'a, I, II>(
    iter: I,
    buf: &mut String,
    pre: &str,
    sep: &str,
    last_punct: &str,
) where
    I: Iterator<Item = &'a II>,
    II: ConstructPrettyCode + 'a,
{
    _put_str_join(
        iter,
        buf,
        pre,
        sep,
        last_punct,
        ConstructPrettyCode::write_pretty_code,
    );
}

pub(crate) fn put_attrs<'a, I, II>(iter: I, buf: &mut String)
where
    I: Iterator<Item = &'a II>,
    II: ConstructWgslCode + 'a,
{
    let prev = buf.len();
    put_str_join(iter, buf, "", "", "");
    if buf.len() > prev {
        // Safety: `buf` is not empty.
        unsafe {
            if buf.chars().last().unwrap_unchecked() != ')' {
                buf.push(' ');
            }
        }
    }
}

pub(crate) fn put_attrs_pretty<'a, I, II>(iter: I, buf: &mut String)
where
    I: Iterator<Item = &'a II>,
    II: ConstructWgslCode + 'a,
{
    let prev = buf.len();
    put_str_join(iter, buf, "", " ", "");
    if buf.len() > prev {
        buf.push(' ');
    }
}

pub(crate) fn get_last_spaces(buf: &str) -> usize {
    buf.chars()
        .rev()
        .enumerate()
        .find_map(|(i, c)| (c != ' ').then_some(i))
        .unwrap_or_default()
}

pub(crate) fn get_last_whitespaces(buf: &str) -> usize {
    buf.chars()
        .rev()
        .enumerate()
        .find_map(|(i, c)| (!c.is_whitespace()).then_some(i))
        .unwrap_or_default()
}

pub(crate) fn get_last_indent(buf: &str) -> usize {
    let mut ns: isize = -1;
    for (i, ch) in buf.chars().rev().enumerate() {
        if ch == '\n' {
            return i - (ns + 1) as usize;
        }
        if ch != ' ' {
            ns = i as isize;
        }
    }
    0
}

pub(crate) fn pushn(buf: &mut String, ch: char, n: usize) {
    for _ in 0..n {
        buf.push(ch);
    }
}

pub(crate) fn popn(buf: &mut String, n: usize) {
    for _ in 0..n {
        buf.pop();
    }
}
