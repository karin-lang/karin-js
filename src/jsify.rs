use std::collections::HashMap;

use crate::*;

use js::*;
use karinc::hir;

#[derive(Clone, Debug, PartialEq)]
pub enum JsifyLog {}

pub struct Jsify {
    logs: Vec<JsifyLog>,
}

impl Jsify {
    pub fn new() -> Jsify {
        Jsify { logs: Vec::new() }
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

    pub fn jsify_fn_decl(&mut self, decl: &hir::FnDecl) -> FnDecl {
        let mut elems = Vec::new();
        for expr in &decl.body.exprs {
            let new_elem = self.jsify_expr(expr, &decl.body.vars);
            elems.push(new_elem);
        }
        let body = Body { arg_len: decl.body.args.len(), elems };
        FnDecl { body }
    }

    pub fn jsify_expr(&mut self, expr: &hir::Expr, vars: &Vec<hir::VarDef>) -> Elem {
        match &expr.kind {
            hir::ExprKind::Literal(literal) => Elem::Literal(literal.clone()),
            hir::ExprKind::VarDef(var_id) => Elem::VarDef(self.jsify_var_def(vars, var_id)),
            _ => unimplemented!(),
        }
    }

    pub fn jsify_var_def(&mut self, vars: &Vec<hir::VarDef>, var_id: &VarId) -> VarDef {
        let var_def = vars.get(var_id.into_usize()).expect("unknown variable id");
        let init = var_def.init.as_ref().map(|expr| Box::new(self.jsify_expr(expr, vars)));
        VarDef { id: *var_id, init }
    }
}
