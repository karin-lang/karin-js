pub mod code;
pub mod js;
pub mod jsify;
pub mod option;
#[cfg(test)]
mod tests;

use code::{Code, build::CodeBuilder};
use js::Js;
use jsify::Jsify;
use karinc::hir::lower::HirLowering;
use karinc::hir::Hir;
use karinc::input::*;
use karinc::log::CompilerLog;
use karinc::lexer::tokenize::Lexer;
use karinc::parser::ast::tltype::TopLevelTypeTable;
use karinc::parser::ast::Ast;
use karinc::parser::{Parser, ParserHakoContext};
use karinc::typesys::constraint::lower::TypeConstraintLowering;
use karinc::typesys::constraint::TypeLog;
use option::CompilerOptions;

pub struct Compiler;

impl Compiler {
    pub fn compile(input: &InputTree, options: &CompilerOptions) -> Output {
        let js = Compiler::jsify(input);
        if options.bundles {
            let mut code_builder = CodeBuilder::new();
            let mut code = Code::new();
            for (each_path, each_item) in &js.items {
                let new_item = code_builder.code_item(each_path, each_item);
                code.append(&new_item);
            }
            let file = OutputFile {
                name: options.root_source_name.clone(),
                ext: OutputFileExt::Js,
                source: Some(code),
            };
            Output {
                files: vec![file],
                logs: Vec::new(), // fix
            }
        } else {
            unimplemented!();
        }
    }

    pub fn jsify(input: &InputTree) -> Js {
        let (hir, hir_lowering_logs) = Compiler::gen_hir(input);
        let mut jsify = Jsify::new();
        jsify.jsify(&hir)
    }

    pub fn gen_hir(input: &InputTree) -> (Hir, Vec<CompilerLog>) {
        // todo: モジュールごとのログ出力を見る
        let (hir_lowering_input, mut top_level_type_table) = Compiler::gen_hir_lowering_input(input);
        let (hir, hir_compiler_logs) = Compiler::hirify(&hir_lowering_input);
        let type_logs = Compiler::check_type(&hir, &mut top_level_type_table);
        (hir, Vec::new())
    }

    pub fn gen_hir_lowering_input(input: &InputTree) -> (HirLoweringInput, TopLevelTypeTable) {
        // todo: ログの種類ごとに要中断か判断を入れる (warning等で中断されないように)
        let mut mods = Vec::new();
        let mut logs = Vec::new();
        let mut top_level_type_table = TopLevelTypeTable::new();
        for each_hako in &input.hakos {
            for each_mod in &each_hako.mods {
                let (new_mod, mut compiler_logs) = Compiler::gen_hir_lowering_mod(each_hako, each_mod, &mut top_level_type_table);
                mods.push(new_mod);
                logs.append(&mut compiler_logs);
            }
        }
        let input = HirLoweringInput { mods, logs };
        (input, top_level_type_table)
    }

    pub fn gen_hir_lowering_mod(hako: &HakoInput, r#mod: &ModInput, top_level_type_table: &mut TopLevelTypeTable) -> (HirLoweringMod, Vec<CompilerLog>) {
        let lexer = Lexer::new();
        // fix: ログの処理方法：レクサログがあったらここで中断するか？
        let (tokens, lexer_logs) = lexer.tokenize(&r#mod.source);
        if !lexer_logs.is_empty() {
            let r#mod = HirLoweringMod { ast: None };
            let logs = lexer_logs.into_iter().map(|v| v.into()).collect();
            return (r#mod, logs);
        }
        let mut hako_context = ParserHakoContext::new(hako.id);
        let parser = Parser::new(&tokens, &mut hako_context);
        let (ast, parser_logs) = parser.parse(r#mod.id, r#mod.path.clone());
        if !parser_logs.is_empty() {
            let r#mod = HirLoweringMod { ast: None };
            let logs = parser_logs.into_iter().map(|v| v.into()).collect();
            return (r#mod, logs);
        }
        top_level_type_table.absorb(&ast);
        let r#mod = HirLoweringMod { ast: Some(ast) };
        (r#mod, Vec::new())
    }

    pub fn hirify(input: &HirLoweringInput) -> (Hir, Vec<CompilerLog>) {
        let mut asts = Vec::new();
        for each_mod in &input.mods {
            if let Some(ast) = &each_mod.ast {
                asts.push(ast);
            }
        }
        let lowering = HirLowering::new(&asts);
        let (hir, logs) = lowering.lower();
        let compiler_logs = logs.into_iter().map(|v| v.into()).collect();
        (hir, compiler_logs)
    }

    pub fn check_type(hir: &Hir, top_level_type_table: &mut TopLevelTypeTable) -> Vec<TypeLog> {
        TypeConstraintLowering::lower(hir, top_level_type_table).1
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirLoweringInput {
    pub mods: Vec<HirLoweringMod>,
    pub logs: Vec<CompilerLog>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirLoweringMod {
    pub ast: Option<Ast>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Output {
    pub files: Vec<OutputFile>,
    pub logs: Vec<CompilerLog>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OutputFile {
    pub name: String,
    pub ext: OutputFileExt,
    pub source: Option<Code>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum OutputFileExt {
    Js,
}
