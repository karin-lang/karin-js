use std::collections::HashMap;

use karinc::lexer::token;
use karinc::parser::ast;
use karinc::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Js {
    pub items: HashMap<ast::Path, Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub id: ItemId,
    pub kind: ItemKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub body: Body,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    pub arg_len: usize,
    pub elems: Vec<Elem>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Elem {
    VarDef(VarDef),
    Literal(token::Literal),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub id: VarId,
    pub init: Option<Box<Elem>>,
}
