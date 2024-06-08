use crate::code::Code;

use karinc::log::CompilerLog;

#[derive(Clone, Debug, PartialEq)]
pub struct Output {
    pub files: Vec<OutputFile>,
    pub logs: Vec<CompilerLog>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OutputFile {
    pub name: String,
    pub ext: OutputFileExt,
    pub source: Option<Code>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum OutputFileExt {
    Js,
}
