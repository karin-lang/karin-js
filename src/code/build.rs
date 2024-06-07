use crate::js::*;

use karinc::parser::ast;

pub struct CodeBuilder;

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder
    }

    pub fn code_item(&mut self, path: &ast::Path, item: &Item) -> String {
        match &item.kind {
            ItemKind::FnDecl(decl) => {
                let name = format!("f${}", path.segments.join("$"));
                let args: Vec<String> = (0..decl.body.arg_len).map(|v| format!("a${v}")).collect();
                let stmts: Vec<String> = decl.body.stmts.iter().map(|stmt| self.code_stmt(stmt)).collect();
                format!("function {}({}){{{}}}", name, args.join(","), stmts.join(";"))
            },
        }
    }

    pub fn code_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Expr(expr) => self.code_expr(expr),
        }
    }

    pub fn code_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(literal) => match literal {
                Literal::Bool { value } => value.to_string(),
            },
        }
    }
}
