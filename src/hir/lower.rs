use std::collections::HashSet;

use resolve::*;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {}

pub struct HirLowering<'a> {
    asts: &'a Vec<ast::Ast>,
    current_mod_path: ast::Path,
    paths: HashSet<ast::Path>,
    body_scope_hierarchy: BodyScopeHierarchy,
    logs: Vec<HirLoweringLog>,
}

impl<'a> HirLowering<'a> {
    pub fn new(asts: &'a Vec<ast::Ast>) -> HirLowering<'a> {
        let mut lowering = HirLowering {
            asts,
            current_mod_path: ast::Path::new(),
            paths: HashSet::new(),
            body_scope_hierarchy: BodyScopeHierarchy::new(),
            logs: Vec::new(),
        };
        lowering.collect();
        lowering
    }

    pub fn collect(&mut self) {
        for each_ast in self.asts {
            self.paths.insert(each_ast.mod_path.clone());
            for each_item in &each_ast.items {
                let new_item_path = each_ast.mod_path.clone().add_segment(&each_item.id.id);
                self.paths.insert(new_item_path);
            }
        }
    }

    pub fn lower(mut self) -> (Hir, Vec<HirLoweringLog>) {
        let mut items = HashMap::new();
        for each_ast in self.asts {
            self.lower_ast(&mut items, each_ast);
        }
        let hir = Hir { items };
        (hir, self.logs)
    }

    pub fn resolve(&mut self, id: &str) -> Option<Expr> {
        if let Some(local_id) = self.resolve_local(id) {
            return Some(Expr::LocalRef(local_id));
        }
        if let Some(path) = self.resolve_item(id) {
            return Some(Expr::PathRef(path));
        }
        None
    }

    pub fn resolve_item(&self, id: &str) -> Option<ast::Path> {
        let path = self.get_item_path(id);
        if self.paths.contains(&path) {
            Some(path)
        } else {
            None
        }
    }

    pub fn resolve_local(&self, id: &str) -> Option<LocalId> {
        self.body_scope_hierarchy.resolve(id)
    }

    pub fn get_item_path(&self, id: &str) -> ast::Path {
        self.current_mod_path.clone().add_segment(id)
    }

    pub fn lower_ast(&mut self, hir_items: &mut HashMap<ast::Path, Item>, ast: &ast::Ast) {
        self.current_mod_path = ast.mod_path.clone();
        for each_item in &ast.items {
            let new_hir_item = self.lower_item(each_item);
            let new_hir_path = self.get_item_path(&each_item.id.id);
            hir_items.insert(new_hir_path, new_hir_item);
        }
    }

    pub fn lower_item(&mut self, item: &ast::Item) -> Item {
        match &item.kind {
            ast::ItemKind::FnDecl(decl) => {
                let hir_decl = self.lower_fn_decl(decl);
                Item::FnDecl(hir_decl)
            },
        }
    }

    pub fn lower_fn_decl(&mut self, decl: &ast::FnDecl) -> FnDecl {
        let args = decl.args.iter().map(|v| self.lower_formal_arg(v)).collect();
        let body = self.lower_body(&decl.body);
        let decl = FnDecl { args, body };
        decl
    }

    pub fn lower_body(&mut self, body: &ast::Body) -> Body {
        self.body_scope_hierarchy.enter_scope();
        let exprs = body.exprs.iter().map(|v| self.lower_expr(v)).collect();
        let locals = self.body_scope_hierarchy.leave_scope();
        Body { exprs, locals }
    }

    pub fn lower_formal_arg(&mut self, arg: &ast::FormalArg) -> LocalId {
        let arg_entity = FormalArg;
        let local = Local::FormalArg(arg_entity);
        self.body_scope_hierarchy.declare(&arg.id.id, local)
    }

    pub fn lower_expr(&mut self, expr: &ast::Expr) -> Expr {
        // todo: 実装
        match &expr.kind {
            ast::ExprKind::Id(id) => self.resolve(&id.id).unwrap(), //fix unwrap()
            ast::ExprKind::VarDecl(decl) => {
                let local = Local::VarDecl(VarDecl { mutable: false }); // fix mutability
                let local_id = self.body_scope_hierarchy.declare(&decl.id.id, local);
                Expr::LocalDecl(local_id)
            },
            _ => unimplemented!(),
        }
    }
}
