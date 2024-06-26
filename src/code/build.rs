use crate::js::*;

use karinc::lexer::token;
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
                let stmts: Vec<String> = decl.body.elems.iter().map(|stmt| self.code_elem(stmt)).collect();
                format!("function {}({}){{{}}}", name, args.join(","), stmts.join(";"))
            },
        }
    }

    pub fn code_elem(&mut self, elem: &Elem) -> String {
        match elem {
            Elem::Literal(literal) => match literal {
                token::Literal::Bool { value } => value.to_string(),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}
