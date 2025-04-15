pub mod known;
pub mod naga_proc;
pub mod path_proc;
pub mod path_tree;
pub mod queue;

use crate::Result;
use naga_proc::NagaProcessor;
use path_proc::PathProcessor;
use std::path::Path as StdPath;

pub struct SemanticAnalyzer {
    pub path_proc: PathProcessor,
    pub naga_proc: NagaProcessor,
}

impl SemanticAnalyzer {
    pub fn new<P>(entry: P) -> Result<Self>
    where
        P: AsRef<StdPath>,
    {
        Ok(Self {
            path_proc: PathProcessor::new(entry.as_ref())?,
            naga_proc: NagaProcessor::new(),
        })
    }

    pub fn import<I, II>(&mut self, fpaths: I) -> Result<()>
    where
        I: Iterator<Item = II>,
        II: AsRef<StdPath>,
    {
        self.path_proc.import(fpaths)
    }

    pub fn process(&mut self) -> Result<()> {
        self.path_proc.process()?;
        self.naga_proc.process(&self.path_proc)?;
        Ok(())
    }
}
