use std::fmt;

use super::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub mod_path: Path,
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Path {
    pub segments: Vec<String>,
}

impl Path {
    pub fn new() -> Path {
        Path { segments: Vec::new() }
    }

    pub fn add_segment(mut self, segment: &str) -> Path {
        self.segments.push(segment.to_string());
        self
    }

    pub fn push_segment(&mut self, segment: &str) {
        self.segments.push(segment.to_string());
    }

    pub fn pop_segment(&mut self) -> Option<String> {
        self.segments.pop()
    }
}

impl From<&str> for Path {
    fn from(value: &str) -> Self {
        Self { segments: value.split("::").map(|v| v.to_string()).collect() }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub id: Id,
    pub kind: ItemKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    FnDecl(FnDecl),
}

#[derive(Clone, PartialEq)]
pub struct Id {
    pub id: String,
    pub span: Span,
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id({:?}, {:?})", self.id, self.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub args: Vec<FormalArg>,
    pub ret_type: Option<Type>,
    pub body: Body,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    pub exprs: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub id: Id,
    pub r#type: Type,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Id(Id),
    FormalArg(Id),
    VarDecl(VarDecl),
    VarInit(VarInit),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub kind: Box<TypeKind>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Id(Id),
    Prim(PrimType),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PrimType {
    Bool,
    I8, I16, I32, I64, Isize,
    U8, U16, U32, U64, Usize,
    F32, F64,
    Char, Str,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub id: Id,
    pub r#type: Option<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarInit {
    pub id: Id,
    pub r#type: Option<Type>,
    pub expr: Box<Expr>,
}
