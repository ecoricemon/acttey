use crate::{Error, Result};
use std::{
    any::TypeId,
    cell::RefCell,
    collections::HashMap,
    env,
    ffi::OsStr,
    fmt::{self, Write},
    fs,
    hash::{BuildHasher, Hash, RandomState},
    path::{Path as StdPath, PathBuf},
};
use syn_locator::Locate;

fn with_path<P>(e: std::io::Error, path: P) -> Error
where
    P: AsRef<StdPath>,
{
    let mut s = String::new();
    let path = path.as_ref();
    write!(&mut s, "file: {path:?}, err: {e}").unwrap();
    s.into()
}

// Using 'env::args()' can be used to find more specific root directory than
// 'env::current_dir()' because 'env::args()' gives us the exact entry file
// path. But it only works on build process, not on macro expansion because
// there's no arguments during macro expansion.
pub(crate) fn to_absolute_path<P>(path: P) -> Result<PathBuf>
where
    P: AsRef<StdPath>,
{
    let path: &StdPath = path.as_ref();
    let abs_path = if path.is_absolute() {
        path.canonicalize().map_err(|e| with_path(e, path))?
    } else {
        env::current_dir()?
            .join(path)
            .canonicalize()
            .map_err(|e| with_path(e, path))?
    };
    Ok(abs_path)
}

pub(crate) fn to_relative_path(path: &StdPath) -> Result<&StdPath> {
    let path: &StdPath = path.as_ref();
    let rel_path = if path.is_relative() {
        path
    } else {
        path.strip_prefix(env::current_dir()?)?
    };
    Ok(rel_path)
}

pub(crate) fn root_dir<P>(path: P) -> Result<PathBuf>
where
    P: AsRef<StdPath>,
{
    let entry = entry_path(path)?;
    let dir = as_dir(&entry)?;
    Ok(dir.to_path_buf())
}

pub(crate) fn as_dir(path: &StdPath) -> Result<&StdPath> {
    if path.is_dir() {
        Ok(path)
    } else {
        path.parent()
            .ok_or(format!("failed to find parent directory of `{path:?}`").into())
    }
}

pub(crate) fn is_mod_rs<P>(path: P) -> bool
where
    P: AsRef<StdPath>,
{
    let path = path.as_ref();
    if path.is_file() {
        path.ends_with("mod.rs") || path.ends_with("lib.rs") || path.ends_with("main.rs")
    } else {
        false
    }
}

/// Returns absolute path of the entry file like `/a/main.rs` or `/a/lib.rs`.
pub(crate) fn entry_path<P>(path: P) -> Result<PathBuf>
where
    P: AsRef<StdPath>,
{
    let abs_path = to_absolute_path(path)?;
    let mut cur = as_dir(&abs_path)?;

    loop {
        for entry in fs::read_dir(cur)? {
            let entry = entry?;
            let path = entry.path();
            let Some(fname) = path.file_name() else {
                continue;
            };
            if fname.eq_ignore_ascii_case("main.rs") || fname.eq_ignore_ascii_case("lib.rs") {
                return Ok(entry.path());
            }
        }
        if let Some(parent) = cur.parent() {
            cur = parent;
        } else {
            break;
        }
    }

    Err(format!("couldn't find entry file for `{cur:?}`").into())
}

/// e.g.
/// - /a/b.rs      -> /a/b
/// - /a/b/mod.rs  -> /a/b
/// - /a/b/lib.rs  -> /a/b
/// - /a/b/main.rs -> /a/b
pub(crate) fn fpath_to_vfpath<P>(fpath: P) -> Result<PathBuf>
where
    P: AsRef<StdPath>,
{
    let fpath: &StdPath = fpath.as_ref();
    if fpath.is_dir() {
        return Err(format!("{:?} is not a rust file", fpath).into());
    }
    if let Some(ext) = fpath.extension() {
        if ext != "rs" {
            return Err(format!("{:?} is not a rust file", fpath).into());
        }
    }

    let temp;
    let dir = fpath.parent().unwrap();
    let fname = fpath.file_name().unwrap();

    let path = if fname == "mod.rs" || fname == "lib.rs" || fname == "main.rs" {
        dir
    } else {
        let fname = os_str_to_str(fname)?;
        temp = dir.join(fname.trim_end_matches(".rs"));
        temp.as_path()
    };

    Ok(path.to_path_buf())
}

/// e.g.
/// /a/b -> /a/b.rs or /a/b/mod.rs
pub(crate) fn vfpath_to_fpath<P>(vfpath: P) -> (PathBuf, PathBuf)
where
    P: AsRef<StdPath>,
{
    let a = vfpath.as_ref().with_extension("rs");
    let b = vfpath.as_ref().join("mod.rs");
    (a, b)
}

pub(crate) fn os_str_to_str(os: &OsStr) -> Result<&str> {
    if let Some(s) = os.to_str() {
        Ok(s)
    } else {
        Err(format!("`{os:?}` is not a UTF-8 literal").into())
    }
}

/// Creates string which looks like "a::b::C" from the given path ignoring
/// leading colon.
pub(crate) fn path_to_string(path: &syn::Path) -> String {
    let mut buf = String::new();
    for segment in &path.segments {
        push_colon_path(&mut buf, &segment.ident);
    }
    buf
}

pub(crate) fn element_type_path_string(ty: &syn::Type) -> Result<String> {
    if let syn::Type::Path(v) = element_type(ty)? {
        Ok(path_to_string(&v.path))
    } else {
        Err(format!("{}: not supported type", ty.location_message()).into())
    }
}

pub(crate) fn element_type(ty: &syn::Type) -> Result<&syn::Type> {
    match ty {
        syn::Type::Path(_) => Ok(ty),
        syn::Type::Array(v) => element_type(&v.elem),
        syn::Type::Slice(v) => element_type(&v.elem),
        _ => Err(format!("{}: not supported type", ty.location_message()).into()),
    }
}

pub(crate) fn push_colon_path<S>(dst: &mut String, add: S)
where
    S: fmt::Display,
{
    if dst.is_empty() {
        write!(dst, "{}", add)
    } else if dst.ends_with("::") {
        write!(dst, "{}", add)
    } else {
        write!(dst, "::{}", add)
    }
    .unwrap();
}

pub(crate) fn assert_finite_loop<K, E>(key: K, err: E)
where
    K: Eq + Hash + fmt::Debug + Clone + 'static,
    E: fmt::Display,
{
    thread_local! {
        static COUNTERS: RefCell<HashMap<TypeId, HashMap<u64, u32>>> = RefCell::new(
            HashMap::new()
        );
        static STATE: RandomState = RandomState::new();
    }

    let tid = TypeId::of::<K>();
    let key_hash = STATE.with(|state| state.hash_one(key.clone()));

    COUNTERS.with_borrow_mut(|counters| {
        let counter = counters.entry(tid).or_default();
        counter
            .entry(key_hash)
            .and_modify(|cnt| {
                *cnt -= 1;
                if *cnt == 0 {
                    panic!("abnormal iterations: msg: {err}, key: {key:?}");
                }
            })
            .or_insert(10);
    })
}

pub(crate) fn call_site() -> proc_macro2::Span {
    proc_macro2::Span::call_site()
}
