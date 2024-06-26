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
    Block(Block),
    VarDef(VarDef),
    Literal(token::Literal),
    If(If),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub elems: Vec<Elem>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub id: VarId,
    pub init: Option<Box<Elem>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub cond: Box<Elem>,
    pub block: Block,
    pub elifs: Vec<Elif>,
    pub r#else: Option<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Elif {
    pub cond: Box<Elem>,
    pub block: Block,
}
