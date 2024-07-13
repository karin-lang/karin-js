#[derive(Clone, Debug, PartialEq)]
pub struct CompilerOptions {
    pub output_root_name: String,
    pub bundles: bool,
    pub module: JsModule,
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsModule {
    Es,
    Common,
}
