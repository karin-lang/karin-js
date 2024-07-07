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
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Block(Block),
    Ret(Ret),
    VarDef(VarDef),
    VarBind(VarBind),
    If(If),
    For(For),
    While(While),
}

impl Stmt {
    pub fn expect_expr(self) -> Expr {
        match self {
            Stmt::Expr(expr) => expr,
            _ => panic!("expected expression"),
        }
    }

    pub fn expect_expr_or_null(self) -> Expr {
        match self {
            Stmt::Expr(expr) => expr,
            _ => Expr::Literal(Literal::Null),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ret {
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Id(Id),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Derived(token::Literal),
    Undefined,
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Id {
    Var(usize),
    Tmp(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub id: Id,
    pub init: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarBind {
    pub id: Id,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub block: Block,
    pub elifs: Vec<Elif>,
    pub r#else: Option<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Elif {
    pub cond: Box<Expr>,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct For {
    pub init: Box<Stmt>,
    pub cond: Expr,
    pub after: Box<Stmt>,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub cond: Expr,
    pub block: Block,
}
