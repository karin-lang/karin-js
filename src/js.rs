use std::collections::HashMap;

use karinc::lexer::token;
use karinc::parser::ast::{self, Path};
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
    SysEmbedded(SysEmbedded),
}

#[derive(Clone, Debug, PartialEq)]
pub enum SysEmbedded {
    StdPrintLn,
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
    Operation(Box<Operation>),
    Literal(Literal),
    Id(Id),
    Path(Path),
    FnCall(FnCall),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operation {
    Unary { operator: ast::UnaryOperator, term: Expr },
    Binary { operator: ast::BinaryOperator, left_term: Expr, right_term: Expr },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Derived(token::Literal),
    Undefined,
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Id {
    FormalArg(usize),
    Var(usize),
    Tmp(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub path: Path,
    pub args: Vec<ActualArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ActualArg {
    pub expr: Expr,
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
