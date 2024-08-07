use std::collections::HashMap;

use crate::code::Code;

use karinc::lexer::token::Span;
use karinc::log::CompilerLog;
use karinc::hir::id::ModId;

#[derive(Clone, Debug, PartialEq)]
pub struct Output {
    pub file: OutputFile,
    pub todos: Vec<(String, Span)>,
    pub logs: HashMap<ModId, Vec<CompilerLog>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OutputFile {
    pub name: String,
    pub ext: String,
    pub source: Option<Code>,
}
