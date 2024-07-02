use crate::js::*;

use karinc::hir::id::NumId;
use karinc::lexer::token;
use karinc::parser::ast;

#[derive(Clone, Debug, PartialEq)]
pub struct BodyScopeHierarchy {
    scopes: Vec<BodyScope>,
}

impl BodyScopeHierarchy {
    pub fn new() -> BodyScopeHierarchy {
        BodyScopeHierarchy { scopes: Vec::new() }
    }

    pub fn get_top_scope(&self) -> &BodyScope {
        self.scopes.last().expect("body scope is not found")
    }

    pub fn get_top_scope_mut(&mut self) -> &mut BodyScope {
        self.scopes.last_mut().expect("body scope is not found")
    }

    pub fn enter_scope(&mut self) {
        let new_scope = BodyScope::new();
        self.scopes.push(new_scope);
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop().expect("body scope is not found");
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BodyScope {
    last_tmp_var_id: usize,
}

impl BodyScope {
    pub fn new() -> BodyScope {
        BodyScope { last_tmp_var_id: 0 }
    }

    pub fn generate_tmp_var_id(&mut self) -> usize {
        let new_id = self.last_tmp_var_id;
        self.last_tmp_var_id += 1;
        new_id
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElemCodeSeq {
    elems: Vec<String>,
}

impl ElemCodeSeq {
    pub fn new() -> ElemCodeSeq {
        ElemCodeSeq { elems: Vec::new() }
    }

    pub fn build(&self) -> String {
        self.elems.join(";")
    }

    pub fn add(&mut self, code: String) {
        self.elems.push(code);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ElemCodeResult {
    Result { result: String },
    ResultWithPrevious { previous: String, result: String },
}

impl ElemCodeResult {
    pub fn from(previous: Option<String>, result: String) -> ElemCodeResult {
        match previous {
            Some(previous) => ElemCodeResult::result_with_previous(previous, result),
            None => ElemCodeResult::result(result),
        }
    }

    pub fn result(result: String) -> ElemCodeResult {
        ElemCodeResult::Result { result }
    }

    pub fn result_with_previous(previous: String, result: String) -> ElemCodeResult {
        ElemCodeResult::ResultWithPrevious { previous, result }
    }

    pub fn get_previous(self) -> Option<String> {
        match self {
            ElemCodeResult::Result { result: _ } => None,
            ElemCodeResult::ResultWithPrevious { previous, result: _ } => Some(previous),
        }
    }

    pub fn get_result(self) -> String {
        match self {
            ElemCodeResult::Result { result } => result,
            ElemCodeResult::ResultWithPrevious { previous: _, result } => result,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockCodeOption {
    None,
    PutLastValue { tmp_var_id: usize },
}

pub struct CodeBuilder {
    body_scope_hierarchy: BodyScopeHierarchy,
}

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder { body_scope_hierarchy: BodyScopeHierarchy::new() }
    }

    // fix
    pub fn code_item(&mut self, path: &ast::Path, item: &Item) -> String {
        String::new()
    }
/*
    pub fn code_item(&mut self, path: &ast::Path, item: &Item) -> String {
        match &item.kind {
            ItemKind::FnDecl(decl) => {
                self.body_scope_hierarchy.enter_scope();
                let name = format!("f${}", path.segments.join("$"));
                let args: Vec<String> = (0..decl.body.arg_len).map(|v| format!("a${v}")).collect();
                let elems: Vec<String> = decl.body.elems.iter().map(|elem| self.code_elem(elem).get_result()).collect();
                self.body_scope_hierarchy.leave_scope();
                format!("function {}({}){{{}}}", name, args.join(","), elems.join(";"))
            },
        }
    }

    pub fn get_body_scope(&self) -> &BodyScope {
        self.body_scope_hierarchy.get_top_scope()
    }

    pub fn get_body_scope_mut(&mut self) -> &mut BodyScope {
        self.body_scope_hierarchy.get_top_scope_mut()
    }

    pub fn enter_body_scope(&mut self) {
        self.body_scope_hierarchy.enter_scope();
    }

    pub fn leave_body_scope(&mut self) {
        self.body_scope_hierarchy.leave_scope();
    }

    pub fn code_block(&mut self, block: &Block, option: BlockCodeOption) -> String {
        let mut code_seq = ElemCodeSeq::new();
        let mut i = 0;
        for elem in &block.elems {
            i += 1;
            let elem_code = match &option {
                BlockCodeOption::PutLastValue { tmp_var_id } if i == block.elems.len() => {
                    let name = format!("t${tmp_var_id}");
                    let value = self.code_elem(elem);
                    // todo: previous を追加する
                    self._code_var_bind(name, value.get_result())
                },
                // todo: previous を追加する
                _ => self.code_elem(elem).get_result(),
            };
            code_seq.add(elem_code);
        }
        // let elems = block.elems.iter().map(|elem| self.code_elem(elem)).collect::<Vec<String>>().join(";");
        format!("{{{}}}", code_seq.build())
    }

    pub fn code_elem(&mut self, elem: &Elem) -> ElemCodeResult {
        match elem {
            Elem::Literal(literal) => match literal {
                token::Literal::Bool { value } => ElemCodeResult::result(value.to_string()),
                _ => unimplemented!(),
            },
            Elem::VarDef(def) => self.code_var_def(def),
            Elem::VarBind(bind) => ElemCodeResult::result(self.code_var_bind(bind)),//fix return type
            Elem::If(r#if) => self.code_if(r#if, false),
            Elem::IfExpr(r#if) => self.code_if(r#if, true),
            _ => unimplemented!(),
        }
    }

    fn _code_var_def(&mut self, name: String, init: Option<String>) -> String {
        let mut code = format!("let {name}");
        if let Some(init) = init {
            code += &format!("={init}");
        }
        code
    }

    pub fn code_var_def(&mut self, def: &VarDef) -> ElemCodeResult {
        let name = format!("v${}", def.id.into_usize());
        let init = def.init.as_ref().map(|elem| self.code_elem(elem));
        let def = self._code_var_def(name, init.as_ref().map(|v| v.get_result().clone()));
        if let Some(result) = init {
            ElemCodeResult::from(result.get_previous(), def)
        } else {
            ElemCodeResult::result(def)
        }
    }

    pub fn code_tmp_var_def(&mut self, init: Option<&Elem>) -> (String, usize) {
        let tmp_var_id = self.body_scope_hierarchy.get_top_scope_mut().generate_tmp_var_id();
        let name = format!("t${}", tmp_var_id);
        let init = init.as_ref().map(|elem| self.code_elem(elem));
        let code = init.as_ref().map(|v| v.0.clone()).unwrap_or(String::new()) + &self._code_var_def(name, init.map(|v| v.1));
        (code, tmp_var_id)
    }

    pub fn _code_var_bind(&mut self, name: String, value: String) -> String {
        format!("{name}={value}")
    }

    pub fn code_var_bind(&mut self, bind: &VarBind) -> String {
        let name = format!("v${}", bind.id.into_usize());
        let value = self.code_elem(&bind.value);
        self._code_var_bind(name, value)
    }

    /*
    let tmp_0;
if (cond) {
    tmp_0 = a;
} else {
    tmp_0 = b;
}
let v = tmp_0;
 */
    fn generate_block_code_option(&mut self, has_if_value: bool) -> BlockCodeOption {
        if has_if_value {
            BlockCodeOption::PutLastValue { tmp_var_id: self.get_body_scope_mut().generate_tmp_var_id() }
        } else {
            BlockCodeOption::None
        }
    }

    pub fn _code_if(&mut self, r#if: &If) -> String {
        let cond = self.code_elem(&r#if.cond);

        let block_code_option = self.generate_block_code_option(r#if.has_value);
        let block = self.code_block(&r#if.block, block_code_option);

        let elifs = r#if.elifs.iter().map(|elif| {
            let block_code_option = self.generate_block_code_option(r#if.has_value);
            self.code_elif(elif, block_code_option)
        }).collect();
        let else_block = r#if.r#else.as_ref().map(|block| {
            let block_code_option = self.generate_block_code_option(r#if.has_value);
            self.code_block(block, block_code_option)
        });
        self.__code_if((cond, block), elifs, else_block)
    }

    pub fn __code_if(&mut self, cond_and_block: (String, String), elifs: Vec<(String, String)>, else_block: Option<String>) -> String {
        let (cond, block) = cond_and_block;
        let mut code = format!("if({cond}){block}");
        let elifs = elifs.iter().map(|(elif_cond, elif_block)| format!("else if({elif_cond}){elif_block}")).collect::<Vec<String>>().join("");
        code += &elifs;
        if let Some(else_block_code) = &else_block {
            let else_code = format!("else{else_block_code}");
            code += &else_code;
        }
        code
    }

    // returns (先行式, 結果式)
    pub fn code_if(&mut self, r#if: &If, is_expr: bool) -> ElemCodeResult {
        // メモ：has_valueがtrueの場合は値を、falseの場合はnullをputする?
        if is_expr {
            let mut code_seq = ElemCodeSeq::new();
            let (var_def_code, tmp_var_id) = self.code_tmp_var_def(None);
            println!("{}", var_def_code);
            code_seq.add(var_def_code);
            code_seq.add(self._code_if(r#if));
            ElemCodeResult {
                previous: code_seq.build(),
                result: format!("t${tmp_var_id}"),
            }
        } else {
            ElemCodeResult {
                previous: String::new(),
                result: self._code_if(r#if),
            }
        }
    }

    pub fn code_elif(&mut self, elif: &Elif, option: BlockCodeOption) -> (String, String) {
        let cond = self.code_elem(&elif.cond);
        let block = self.code_block(&elif.block, option);
        (cond, block)
    }
*/
}
