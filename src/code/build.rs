use karinc::{lexer::token, parser::ast::{self, Path}};

use crate::js::*;

pub struct CodeBuilder;

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder
    }

    pub fn code_item(&mut self, path: &Path, item: &Item) -> String {
        match &item.kind {
            ItemKind::SysEmbedded(sys_embedded) => match sys_embedded {
                SysEmbedded::StdPrintLn => "function g$std$main$println(a$0){console.log(a$0);}".to_string(),
            },
            ItemKind::FnDecl(decl) => {
                let name = format!("g${}", path.segments.join("$"));
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
            Stmt::Ret(ret) => self.code_ret(ret),
            Stmt::VarDef(def) => self.code_var_def(def),
            Stmt::VarBind(bind) => self.code_var_bind(bind),
            Stmt::If(r#if) => self.code_if(r#if),
            Stmt::For(_) => unimplemented!(),
            Stmt::While(r#while) => self.code_while(r#while),
        }
    }

    pub fn code_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Operation(operation) => self.code_operation(operation),
            Expr::Literal(literal) => self.code_literal(literal),
            Expr::Id(id) => self.code_id(id),
            Expr::Path(path) => self.code_path(path),
            Expr::FnCall(call) => self.code_fn_call(call),
            Expr::Throw(literal) => self.code_throw(literal),
        }
    }

    pub fn code_operation(&mut self, operation: &Operation) -> String {
        match operation {
            Operation::Unary { operator, term } => {
                let term_code = self.code_expr(term);
                match operator {
                    ast::UnaryOperator::Not => format!("(!{term_code})"),
                    ast::UnaryOperator::Void => self.code_expr(&Expr::Literal(Literal::Null)),
                }
            },
            Operation::Binary { operator, left_term, right_term } => {
                let left_term_code = self.code_expr(left_term);
                let right_term_code = self.code_expr(right_term);
                let operator_code = match operator {
                    ast::BinaryOperator::Add => "+",
                    ast::BinaryOperator::Sub => "-",
                    ast::BinaryOperator::Mul => "*",
                    ast::BinaryOperator::Div => "/",
                };
                format!("({left_term_code}{operator_code}{right_term_code})")
            },
        }
    }

    pub fn code_literal(&mut self, literal: &Literal) -> String {
        match literal {
            Literal::Derived(derived) => match derived {
                token::Literal::Void => "null".to_string(),
                token::Literal::Bool { value } => value.to_string(),
                token::Literal::Int { base, int_digits, r#type: _ } => {
                    let base_code = self.code_base(base);
                    format!("{base_code}{int_digits}")
                },
                token::Literal::Float { digits, r#type: _ } => match digits {
                    Some(digits) => format!("{}.{}", digits.int, digits.fraction),
                    None => self.code_expr(&Expr::Literal(Literal::Null)),
                },
                token::Literal::Char { value } => match value {
                    Some(value) => {
                        let value_code = self.code_escseq(&value.to_string());
                        format!("'{value_code}'")
                    },
                    None => self.code_literal(&Literal::Null),
                },
                token::Literal::Str { value } => {
                    let value_code = self.code_escseq(value);
                    format!("'{value_code}'")
                },
                _ => unimplemented!(),
            },
            Literal::Undefined => "undefined".to_string(),
            Literal::Null => "null".to_string(),
        }
    }

    pub fn code_base(&mut self, base: &token::Base) -> String {
        let code = match base {
            token::Base::Bin => "0b",
            token::Base::Oct => "0o",
            token::Base::Dec => "",
            token::Base::Hex => "0x",
        };
        code.to_string()
    }

    pub fn code_escseq(&mut self, value: &String) -> String {
        let mut code = String::new();
        for ch in value.chars() {
            let new_ch = match ch {
                '\\' => r"\\",
                '\'' => r"\'",
                '"' => "\\\"",
                '\0' => r"\0",
                '\n' => r"\n",
                '\r' => r"\r",
                '\t' => r"\t",
                _ => &ch.to_string(),
            };
            code += new_ch;
        }
        code
    }

    pub fn code_id(&mut self, id: &Id) -> String {
        match id {
            Id::FormalArg(id) => format!("a${id}"),
            Id::Var(id) => format!("v${id}"),
            Id::Tmp(id) => format!("t${id}"),
        }
    }

    pub fn code_path(&mut self, path: &Path) -> String {
        let segments = path.segments.join("$");
        format!("g${segments}")
    }

    pub fn code_fn_call(&mut self, call: &FnCall) -> String {
        let path = self.code_path(&call.path);
        let args: Vec<String> = call.args.iter().map(|arg| self.code_expr(&arg.expr)).collect();
        format!("{path}({})", args.join(","))
    }

    pub fn code_throw(&mut self, literal: &Literal) -> String {
        let literal_code = self.code_literal(literal);
        format!("throw {literal_code}")
    }

    pub fn code_block(&mut self, block: &Block) -> String {
        let stmts: Vec<String> = block.stmts.iter().map(|stmt| self.code_stmt(stmt)).collect();
        format!("{{{}}}", stmts.join(";"))
    }

    pub fn code_ret(&mut self, ret: &Ret) -> String {
        let value = self.code_expr(&ret.value);
        format!("return {value}")
    }

    pub fn code_var_def(&mut self, def: &VarDef) -> String {
        let id = self.code_id(&def.id);
        let mut code = format!("let {id}");
        if let Some(init) = &def.init {
            let init_code = self.code_expr(init);
            code += &format!("={init_code}");
        }
        code
    }

    pub fn code_var_bind(&mut self, bind: &VarBind) -> String {
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

    pub fn code_while(&mut self, r#while: &While) -> String {
        let cond = self.code_expr(&r#while.cond);
        let block = self.code_block(&r#while.block);
        format!("while({cond}){block}")
    }
}
