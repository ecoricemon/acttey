pub mod back;
pub mod front;

use front::{semantic::SemanticAnalyzer, util};
#[doc(hidden)]
pub use naga::valid::Capabilities;

pub fn translate_to_wgsl<I, II>(paths: I, capability: Capabilities) -> Result<String>
where
    I: Iterator<Item = II> + Clone,
    II: AsRef<std::path::Path>,
{
    let mut sem = translate(paths)?;
    let code = back::wgsl::write_string(&mut sem.naga_proc.module, capability)?;
    Ok(code)
}

fn translate<I, II>(paths: I) -> Result<SemanticAnalyzer>
where
    I: Iterator<Item = II> + Clone,
    II: AsRef<std::path::Path>,
{
    // Validates entry file path.
    let mut entry = None;
    for path in paths.clone() {
        match util::entry_path(&path) {
            Ok(path) => match &entry {
                Some(entry) if entry == &path => {}
                Some(entry) => {
                    return Err(format!(
                        "detected multiple entry files: `{entry:?}` and `{path:?}`"
                    )
                    .into());
                }
                None => entry = Some(path),
            },
            Err(e) => return Err(e),
        }
    }
    let Some(entry) = entry else {
        return Err("failed to find entry file".into());
    };

    let mut sem = SemanticAnalyzer::new(&entry)?;
    sem.import(paths)?;
    sem.process()?;

    Ok(sem)
}

pub type Result<T> = std::result::Result<T, Error>;
pub type Error = Box<dyn std::error::Error + Send + Sync>;
