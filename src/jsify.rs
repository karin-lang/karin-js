use std::collections::HashMap;

use crate::*;
use crate::js::*;

use karinc::lexer::token;
use karinc::hir;

#[derive(Clone, Debug, PartialEq)]
pub enum JsifyLog {}

#[derive(Clone, Debug, PartialEq)]
pub struct BodyScope<'a> {
    body: &'a hir::Body,
    last_tmp_var_index: usize,
}

impl<'a> BodyScope<'a> {
    pub fn new(body: &'a hir::Body) -> BodyScope<'a> {
        BodyScope { body, last_tmp_var_index: 0 }
    }

    pub fn get_body(&self) -> &'a hir::Body {
        self.body
    }

    pub fn generate_tmp_var_id(&mut self) -> usize {
        let new_id = self.last_tmp_var_index;
        self.last_tmp_var_index += 1;
        new_id
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtSeq {
    stmts: Vec<Stmt>,
}

impl StmtSeq {
    pub fn new() -> StmtSeq {
        StmtSeq { stmts: Vec::new() }
    }

    pub fn append(&mut self, stmt_seq: StmtSeq) {
        self.stmts.append(&mut stmt_seq.into());
    }

    pub fn push(&mut self, stmt: Stmt) {
        self.stmts.push(stmt);
    }

    // 先行文をシーケンスに追加して、結果文を呼び出し元で利用できるように結果文を返す
    pub fn push_previous_stmts(&mut self, result: StmtResult) -> Stmt {
        if let Some(previous) = result.previous {
            self.append(previous);
        }
        result.result
    }
}

impl From<Vec<Stmt>> for StmtSeq {
    fn from(value: Vec<Stmt>) -> Self {
        Self { stmts: value }
    }
}

impl Into<Vec<Stmt>> for StmtSeq {
    fn into(self) -> Vec<Stmt> {
        self.stmts
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtResult {
    pub result: Stmt,
    pub previous: Option<StmtSeq>,
}

impl StmtResult {
    pub fn new(result: Stmt) -> StmtResult {
        StmtResult { result, previous: None }
    }

    pub fn new_with_previous(result: Stmt, previous: StmtSeq) -> StmtResult {
        StmtResult { result, previous: Some(previous) }
    }

    // 結果式として null 式が要求された場合は引数 stmt を先行式とする
    // Karin における式を JS における文に変換する際、式であることを要求された場合に利用する
    pub fn new_or_null(stmt: Stmt, expect_null: bool) -> StmtResult {
        if expect_null {
            let result = Stmt::Expr(Expr::Literal(Literal::Null));
            StmtResult::new_with_previous(result, vec![stmt].into())
        } else {
            StmtResult::new(stmt)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BlockLastBind {
    None,
    LastBind { tmp_id: usize },
}

pub struct Jsify<'a> {
    type_table: &'a TypeConstraintTable,
    logs: Vec<JsifyLog>,
    pub test_stmts: Vec<Stmt>,
}

impl<'a> Jsify<'a> {
    pub fn new(type_table: &'a TypeConstraintTable) -> Jsify<'a> {
        Jsify {
            type_table,
            logs: Vec::new(),
            test_stmts: Vec::new(),
        }
    }

    pub fn get_logs(&self) -> &Vec<JsifyLog> {
        &self.logs
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

    pub fn jsify_body(&mut self, body: &hir::Body) -> Body {
        let mut body_scope = BodyScope::new(body);
        let mut stmt_seq = StmtSeq::new();
        for expr in &body.exprs {
            let new_stmt = self.jsify_expr(&mut body_scope, &mut stmt_seq, expr, false);
            stmt_seq.push(new_stmt);
        }
        let stmts = stmt_seq.into();
        Body { arg_len: body.args.len(), stmts }
    }

    pub fn jsify_fn_decl(&mut self, decl: &hir::FnDecl) -> FnDecl {
        let body = self.jsify_body(&decl.body);
        FnDecl { body }
    }

    // HIR の式を JSify して、先行文のみをステートメントシーケンスに追加する
    // 呼び出し元で結果文を利用できるように結果文を返す
    // 注: Karin における式を JS における文に変換する場合、引数 expect_expr に応じて先行式を生成すること
    //     ※必要に応じて StmtResult::new_or_null() 関数を利用する
    pub fn jsify_expr(&mut self, body_scope: &mut BodyScope, stmt_seq: &mut StmtSeq, expr: &hir::Expr, expect_expr: bool) -> Stmt {
        let result = match &expr.kind {
            hir::ExprKind::Operation(operation) => {
                let js_operation = self.jsify_operation(body_scope, stmt_seq, operation);
                let result = Stmt::Expr(Expr::Operation(Box::new(js_operation)));
                StmtResult::new(result)
            },
            hir::ExprKind::Block(block) => {
                let js_block = self.jsify_block(body_scope, block, BlockLastBind::None);
                let stmt = Stmt::Block(js_block);
                StmtResult::new_or_null(stmt, expect_expr)
            },
            hir::ExprKind::Literal(literal) => {
                let js_literal = self.jsify_literal(literal);
                let result = Stmt::Expr(Expr::Literal(js_literal));
                StmtResult::new(result)
            },
            hir::ExprKind::TopLevelRef(_, path) => {
                let result = Stmt::Expr(Expr::Path(path.clone()));
                StmtResult::new(result)
            },
            hir::ExprKind::LocalRef(local_id) => {
                let id = match local_id {
                    LocalId::FormalArg(formal_arg_id) => Id::FormalArg(formal_arg_id.into_usize()),
                    LocalId::Var(var_id) => Id::Var(var_id.into_usize()),
                };
                let result = Stmt::Expr(Expr::Id(id));
                StmtResult::new(result)
            },
            hir::ExprKind::Ret(ret) => {
                let js_value = self.jsify_expr(body_scope, stmt_seq, &ret.value, true).expect_expr();
                let js_ret = Ret { value: js_value };
                let stmt = Stmt::Ret(js_ret);
                StmtResult::new_or_null(stmt, expect_expr)
            },
            hir::ExprKind::FnCall(call) => {
                let stmt = self.jsify_call(body_scope, stmt_seq, call)
                    .map(|js_call| Stmt::Expr(Expr::FnCall(js_call)))
                    .unwrap_or(Stmt::Expr(Expr::Literal(Literal::Null)));
                StmtResult::new(stmt)
            },
            hir::ExprKind::VarDef(var_id) => {
                let js_def = self.jsify_var_def(body_scope, stmt_seq, *var_id);
                let stmt = Stmt::VarDef(js_def);
                StmtResult::new_or_null(stmt, expect_expr)
            },
            hir::ExprKind::VarBind(bind) => {
                let js_bind = self.jsify_var_bind(body_scope, stmt_seq, bind);
                let stmt = Stmt::VarBind(js_bind);
                StmtResult::new_or_null(stmt, expect_expr)
            },
            hir::ExprKind::If(r#if) => self.jsify_if(body_scope, stmt_seq, r#if, expect_expr),
            hir::ExprKind::For(r#for) => self.jsify_for(body_scope, stmt_seq, r#for, expect_expr),
            hir::ExprKind::Unknown => {
                let result = Stmt::Expr(Expr::Literal(Literal::Null));
                StmtResult::new(result)
            },
        };
        stmt_seq.push_previous_stmts(result)
    }

    pub fn jsify_operation(&mut self, body_scope: &mut BodyScope, stmt_seq: &mut StmtSeq, operation: &hir::Operation) -> Operation {
        println!("{operation:?}");
        match operation {
            hir::Operation::Unary { operator, term } => Operation::Unary {
                operator: *operator,
                term: self.jsify_expr(body_scope, stmt_seq, term, true).expect_expr(),
            },
            hir::Operation::Binary { operator, left_term, right_term } => Operation::Binary {
                operator: *operator,
                left_term: self.jsify_expr(body_scope, stmt_seq, left_term, true).expect_expr(),
                right_term: self.jsify_expr(body_scope, stmt_seq, right_term, true).expect_expr(),
            },
        }
    }

    pub fn jsify_literal(&mut self, literal: &token::Literal) -> Literal {
        Literal::Derived(literal.clone())
    }

    pub fn jsify_block(&mut self, body_scope: &mut BodyScope, block: &hir::Block, last_bind: BlockLastBind) -> Block {
        let mut stmt_seq = StmtSeq::new();
        let mut i = 0;
        for expr in &block.exprs {
            i += 1;
            let new_stmt = self.jsify_expr(body_scope, &mut stmt_seq, expr, true);
            if i == block.exprs.len() {
                match &last_bind {
                    BlockLastBind::None => stmt_seq.push(new_stmt),
                    BlockLastBind::LastBind { tmp_id } => {
                        let value = Box::new(new_stmt.expect_expr_or_null());
                        let bind = VarBind { id: Id::Tmp(*tmp_id), value };
                        let new_stmt = Stmt::VarBind(bind);
                        stmt_seq.push(new_stmt);
                    },
                }
            } else {
                stmt_seq.push(new_stmt);
            }
        }
        Block { stmts: stmt_seq.into() }
    }

    pub fn jsify_call(&mut self, body_scope: &mut BodyScope, stmt_seq: &mut StmtSeq, call: &hir::FnCall) -> Option<FnCall> {
        match &call.r#fn {
            Some(item_id) => {
                let (_, path) = item_id;
                let args = call.args
                    .iter()
                    .map(|arg| {
                        let expr = self.jsify_expr(body_scope, stmt_seq, &arg.expr, true).expect_expr();
                        ActualArg { expr }
                    }).collect();
                let js_call = FnCall { path: path.clone(), args };
                Some(js_call)
            },
            None => None
        }
    }

    pub fn jsify_var_def(&mut self, body_scope: &mut BodyScope, stmt_seq: &mut StmtSeq, var_id: VarId) -> VarDef {
        let body = body_scope.get_body();
        let def = body.vars.get(var_id.into_usize()).expect("unknown variable id");
        let init = match &def.init {
            Some(init) => {
                let expr = self.jsify_expr(body_scope, stmt_seq, init, true).expect_expr();
                Some(expr)
            },
            None => None,
        };
        let id = Id::Var(var_id.into_usize());
        VarDef { id, init }
    }

    pub fn jsify_var_bind(&mut self, body_scope: &mut BodyScope, stmt_seq: &mut StmtSeq, bind: &hir::VarBind) -> VarBind {
        let id = Id::Var(bind.var_id.into_usize());
        let value = self.jsify_expr(body_scope, stmt_seq, &bind.value, true).expect_expr();
        VarBind { id, value: Box::new(value) }
    }

    pub fn jsify_if(&mut self, body_scope: &mut BodyScope, stmt_seq: &mut StmtSeq, r#if: &hir::If, expect_expr: bool) -> StmtResult {
        let cond = {
            let stmt = self.jsify_expr(body_scope, stmt_seq, &r#if.cond, true);
            Box::new(stmt.expect_expr())
        };
        let block_last_bind = if expect_expr {
            let tmp_id = body_scope.generate_tmp_var_id();
            BlockLastBind::LastBind { tmp_id }
        } else {
            BlockLastBind::None
        };
        let block = self.jsify_block(body_scope, &r#if.block, block_last_bind);
        let elifs = r#if.elifs.iter().map(|elif| self.jsify_elif(body_scope, stmt_seq, elif, expect_expr, block_last_bind)).collect();
        let r#else = r#if.r#else.as_ref().map(|block| self.jsify_block(body_scope, block, block_last_bind));

        let js_if = If { cond, block, elifs, r#else };
        match block_last_bind {
            BlockLastBind::None => {
                let result = Stmt::If(js_if);
                StmtResult::new(result)
            },
            BlockLastBind::LastBind { tmp_id } => {
                let result = Stmt::Expr(Expr::Id(Id::Tmp(tmp_id)));
                let previous = vec![
                    Stmt::VarDef(
                        VarDef {
                            id: Id::Tmp(tmp_id),
                            init: None,
                        },
                    ),
                    Stmt::If(js_if),
                ];
                StmtResult::new_with_previous(result, previous.into())
            },
        }
    }

    pub fn jsify_elif(&mut self, body_scope: &mut BodyScope, stmt_seq: &mut StmtSeq, elif: &hir::Elif, expect_expr: bool, block_last_bind: BlockLastBind) -> Elif {
        let cond = {
            let stmt = self.jsify_expr(body_scope, stmt_seq, &elif.cond, expect_expr);
            Box::new(stmt.expect_expr())
        };
        let block = self.jsify_block(body_scope, &elif.block, block_last_bind);
        Elif { cond, block }
    }

    pub fn jsify_for(&mut self, body_scope: &mut BodyScope, stmt_seq: &mut StmtSeq, r#for: &hir::For, expect_expr: bool) -> StmtResult {
        let block_last_bind: BlockLastBind = if expect_expr {
            let tmp_id = body_scope.generate_tmp_var_id();
            BlockLastBind::LastBind { tmp_id }
        } else {
            BlockLastBind::None
        };
        match &r#for.kind {
            hir::ForKind::Endless => {
                let cond = Expr::Literal(Literal::Derived(token::Literal::Bool { value: true }));
                let block = self.jsify_block(body_scope, &r#for.block, block_last_bind);
                let js_while = While { cond, block };

                match block_last_bind {
                    BlockLastBind::None => {
                        let result = Stmt::While(js_while);
                        StmtResult::new(result)
                    },
                    BlockLastBind::LastBind { tmp_id } => {
                        let result = Stmt::Expr(Expr::Id(Id::Tmp(tmp_id)));
                        let previous = vec![
                            Stmt::VarDef(
                                VarDef {
                                    id: Id::Tmp(tmp_id),
                                    init: None,
                                },
                            ),
                            Stmt::While(js_while),
                        ];
                        StmtResult::new_with_previous(result, previous.into())
                    },
                }
            },
            hir::ForKind::Cond { cond } => {
                let cond = self.jsify_expr(body_scope, stmt_seq, cond, true).expect_expr();
                let block = self.jsify_block(body_scope, &r#for.block, block_last_bind);
                let js_while = While { cond, block };

                match block_last_bind {
                    BlockLastBind::None => {
                        let result = Stmt::While(js_while);
                        StmtResult::new(result)
                    },
                    BlockLastBind::LastBind { tmp_id } => {
                        let result = Stmt::Expr(Expr::Id(Id::Tmp(tmp_id)));
                        let previous = vec![
                            Stmt::VarDef(
                                VarDef {
                                    id: Id::Tmp(tmp_id),
                                    init: None,
                                },
                            ),
                            Stmt::While(js_while),
                        ];
                        StmtResult::new_with_previous(result, previous.into())
                    },
                }
            },
            hir::ForKind::Range { index: _, range: _ } => unimplemented!(),
        }
    }
}
