use karinc::{lexer::token, parser::ast::Path};

use crate::js::*;

pub struct CodeBuilder;

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder
    }

    pub fn code_item(&mut self, path: &Path, item: &Item) -> String {
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
            Stmt::Block(block) => self.code_block(block),
            Stmt::VarDef(def) => self.code_var_def(def),
            Stmt::VarBind(bind) => self.code_var_bind(bind),
            Stmt::If(r#if) => self.code_if(r#if),
        }
    }

    pub fn code_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(literal) => self.code_literal(literal),
            Expr::Id(id) => self.code_id(id),
        }
    }

    pub fn code_literal(&mut self, literal: &Literal) -> String {
        match literal {
            Literal::Derived(derived) => match derived {
                token::Literal::Bool { value } => value.to_string(),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn code_id(&mut self, id: &Id) -> String {
        match id {
            Id::Var(id) => format!("v${id}"),
            Id::Tmp(id) => format!("t${id}"),
        }
    }

    pub fn code_block(&mut self, block: &Block) -> String {
        let stmts: Vec<String> = block.stmts.iter().map(|stmt| self.code_stmt(stmt)).collect();
        format!("{{{}}}", stmts.join(";"))
    }

    fn code_var_def(&mut self, def: &VarDef) -> String {
        let id = self.code_id(&def.id);
        let mut code = format!("let {id}");
        if let Some(init) = &def.init {
            let init_code = self.code_expr(init);
            code += &format!("={init_code}");
        }
        code
    }

    fn code_var_bind(&mut self, bind: &VarBind) -> String {
        let id = self.code_id(&bind.id);
        let value = self.code_expr(&bind.value);
        format!("{id}={value}")
    }

    pub fn code_if(&mut self, r#if: &If) -> String {
        let cond = self.code_expr(&r#if.cond);
        let block = self.code_block(&r#if.block);
        let mut code = format!("if({cond}){block}");
        for elif in &r#if.elifs {
            let elif_cond = self.code_expr(&elif.cond);
            let elif_block = self.code_block(&elif.block);
            let elif_code = format!("else if({elif_cond}){elif_block}");
            code += &elif_code;
        }
        if let Some(r#else) = &r#if.r#else {
            let else_block = self.code_block(&r#else);
            code += &format!("else{else_block}");
        }
        code
    }
}
