#[derive(Clone, Debug, PartialEq)]
pub struct CompilerOptions {
    pub root_source_name: String,
    pub bundles: bool,
    pub module: JsModule,
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsModule {
    Es,
    Common,
}
