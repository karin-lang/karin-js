#[cfg(test)]
mod jsify;

use crate::code::Code;
use crate::option::{CompilerOptions, JsModule};
use crate::{Compiler, Output, OutputFile, OutputFileExt};

use karinc::{hir::id::*, input::*};
use maplit::hashmap;

#[test]
fn compiles() {
    let input = InputTree {
        hakos: vec![
            InputHako {
                id: HakoId::new(0),
                mods: vec![
                    InputMod {
                        id: ModId::new(0, 0),
                        path: "my_hako::my_mod".into(),
                        source: "fn f(a bool,b bool){}".to_string(),
                        submods: Vec::new(),
                    },
                ],
            },
        ],
    };
    let options = CompilerOptions {
        root_source_name: "index".to_string(),
        bundles: true,
        module: JsModule::Es,
    };
    let output = Compiler::compile(&input, &options);
    assert_eq!(
        output,
        Output {
            files: vec![
                OutputFile {
                    name: "index".to_string(),
                    ext: OutputFileExt::Js,
                    source: Some(
                        Code {
                            source: "function f$my_hako$my_mod$f(a$0,a$1){}".to_string(),
                        },
                    ),
                },
            ],
            logs: hashmap! {
                ModId::new(0, 0) => Vec::new(),
            },
        },
    );
}
