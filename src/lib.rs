pub mod code;
pub mod js;
pub mod jsify;
pub mod option;
pub mod output;
#[cfg(test)]
mod tests;

use std::collections::HashMap;

use crate::code::Code;
use crate::code::build::CodeBuilder;
use crate::js::Js;
use crate::jsify::Jsify;
use crate::option::*;
use crate::output::*;

use karinc::hir::id::*;
use karinc::hir::lower::HirLowering;
use karinc::hir::Hir;
use karinc::log::CompilerLog;
use karinc::input::*;
use karinc::lexer::tokenize::Lexer;
use karinc::parser::ast::tltype::TopLevelTypeTable;
use karinc::parser::ast::Ast;
use karinc::parser::{Parser, ParserHakoContext};
use karinc::typesys::constraint::lower::TypeConstraintLowering;
use karinc::typesys::constraint::TypeConstraintTable;

pub struct Compiler;

impl Compiler {
    pub fn compile(input: &InputTree, options: &CompilerOptions) -> Output {
        let (js, logs) = Compiler::jsify(input);
        let mut code_builder = CodeBuilder::new();
        let mut code = Code::new();
        for (each_path, each_item) in &js.items {
            let new_item = code_builder.code_item(each_path, each_item);
            code.append(&new_item);
        }
        let file = OutputFile {
            name: options.output_root_name.clone(),
            ext: "js".to_string(),
            source: Some(code),
        };
        Output { file, logs }
    }

    pub fn jsify(input: &InputTree) -> (Js, HashMap<ModId, Vec<CompilerLog>>) {
        let (hir, type_table, compiler_logs) = Compiler::gen_hir(input);
        let mut jsify = Jsify::new(&type_table);
        let js = jsify.jsify(&hir);
        (js, compiler_logs)
    }

    pub fn gen_hir(input: &InputTree) -> (Hir, TypeConstraintTable, HashMap<ModId, Vec<CompilerLog>>) {
        let (asts, mut top_level_type_table, mut logs) = Compiler::gen_hir_lowering_input(input);
        let hir = Compiler::hirify(&asts, &mut logs);
        let type_table = Compiler::check_type(&hir, &mut top_level_type_table, &mut logs);
        (hir, type_table, logs)
    }

    // Generate HIR modules and type table from input tree.
    pub fn gen_hir_lowering_input(input: &InputTree) -> (Vec<Ast>, TopLevelTypeTable, HashMap<ModId, Vec<CompilerLog>>) {
        let mut asts = Vec::new();
        let mut logs = HashMap::new();
        let mut last_body_id = 0;
        let mut top_level_type_table = TopLevelTypeTable::new();
        for each_hako in &input.hakos {
            Compiler::gen_hir_lowering_input_of_mods(&mut asts, &mut logs, each_hako, &each_hako.mods, &mut last_body_id, &mut top_level_type_table)
        }
        (asts, top_level_type_table, logs)
    }

    pub fn gen_hir_lowering_input_of_mods(
        asts: &mut Vec<Ast>,
        logs: &mut HashMap<ModId, Vec<CompilerLog>>,
        hako: &InputHako,
        mods: &Vec<InputMod>,
        last_body_id: &mut usize,
        top_level_type_table: &mut TopLevelTypeTable,
    ) {
        for each_mod in mods {
            let (ast, compiler_logs) = Compiler::gen_hir_lowering_mod(hako, each_mod, last_body_id, top_level_type_table);
            logs.insert(ast.mod_id, compiler_logs);
            asts.push(ast);
            // サブモジュールを再帰的に変換処理する
            Compiler::gen_hir_lowering_input_of_mods(asts, logs, hako, &each_mod.submods, last_body_id, top_level_type_table);
        }
    }

    // Generate HIR module from module input.
    pub fn gen_hir_lowering_mod(hako: &InputHako, r#mod: &InputMod, last_body_id: &mut usize, top_level_type_table: &mut TopLevelTypeTable) -> (Ast, Vec<CompilerLog>) {
        let mut logs = Vec::new();

        let lexer = Lexer::new();
        let (tokens, lexer_logs) = lexer.tokenize(&r#mod.source);
        if !lexer_logs.is_empty() {
            let lexer_logs = &mut lexer_logs.into_iter().map(|v| v.into()).collect();
            logs.append(lexer_logs);
        }

        let mut hako_context = ParserHakoContext::new(hako.id);
        let parser = Parser::new(&tokens, &mut hako_context, last_body_id);
        let (ast, parser_logs) = parser.parse(r#mod.id, r#mod.path.clone());
        if !parser_logs.is_empty() {
            let parser_logs = &mut parser_logs.into_iter().map(|v| v.into()).collect();
            logs.append(parser_logs);
        }

        top_level_type_table.absorb(&ast);
        (ast, logs)
    }

    pub fn hirify(asts: &Vec<Ast>, logs: &mut HashMap<ModId, Vec<CompilerLog>>) -> Hir {
        let asts = asts.iter().map(|v| v).collect();
        let lowering = HirLowering::new(&asts);
        let (hir, hir_lowering_logs) = lowering.lower();
        for (each_mod_id, each_logs) in hir_lowering_logs {
            let mut compiler_logs = each_logs.iter().map(|v| v.clone().into()).collect();
            match logs.get_mut(&each_mod_id) {
                Some(v) => v.append(&mut compiler_logs),
                None => {
                    let _ = logs.insert(each_mod_id, compiler_logs);
                },
            }
        }
        hir
    }

    pub fn check_type(hir: &Hir, top_level_type_table: &mut TopLevelTypeTable, logs: &mut HashMap<ModId, Vec<CompilerLog>>) -> TypeConstraintTable {
        let (type_table, type_logs) = TypeConstraintLowering::lower(hir, top_level_type_table);
        for (each_mod_id, each_logs) in type_logs {
            let mut compiler_logs = each_logs.iter().map(|v| v.clone().into()).collect();
            match logs.get_mut(&each_mod_id) {
                Some(v) => v.append(&mut compiler_logs),
                None => {
                    let _ = logs.insert(each_mod_id, compiler_logs);
                },
            }
        }
        type_table
    }
}
