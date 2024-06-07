use std::collections::HashMap;

use crate::*;

use js::*;
use karinc::hir;
use karinc::lexer::token;

#[derive(Clone, Debug, PartialEq)]
pub enum JsifyLog {}

pub struct Jsify;

impl Jsify {
    pub fn new() -> Jsify {
        Jsify
    }

    pub fn jsify(&mut self, hir: &hir::Hir) -> Js {
        let mut items = HashMap::new();
        for (each_path, each_item) in &hir.items {
            let new_item = self.jsify_item(&each_item);
            items.insert(each_path.clone(), new_item);
        }
        Js { items }
    }

    pub fn jsify_item(&mut self, item: &hir::Item) -> Item {
        let kind = match &item.kind {
            hir::ItemKind::FnDecl(decl) => ItemKind::FnDecl(self.jsify_fn_decl(decl)),
        };
        Item { id: item.id, kind }
    }

    pub fn jsify_fn_decl(&mut self, decl: &hir::FnDecl) -> FnDecl {
        let mut stmts = Vec::new();
        for expr in &decl.body.exprs {
            let new_stmt = self.jsify_expr(expr);
            stmts.push(new_stmt);
        }
        let body = Body { arg_len: decl.body.args.len(), stmts };
        FnDecl { body }
    }

    pub fn jsify_expr(&mut self, expr: &hir::Expr) -> Stmt {
        match &expr.kind {
            hir::ExprKind::Literal(literal) => Stmt::Expr(Expr::Literal(self.jsify_literal(literal))),
            _ => unimplemented!(),
        }
    }

    pub fn jsify_literal(&mut self, literal: &token::Literal) -> Literal {
        match literal {
            token::Literal::Bool { value } => Literal::Bool { value: *value },
            _ => unimplemented!(),
        }
    }
}
