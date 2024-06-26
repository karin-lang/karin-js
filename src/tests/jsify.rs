use karinc::lexer::token::{self, Span};
use karinc::parser::ast::{self, Id};
use karinc::hir;
use karinc::hir::id::*;
use maplit::hashmap;

use crate::js::*;
use crate::jsify::Jsify;

/* item */

#[test]
fn jsifies_items() {
    let mut jsify = Jsify::new();
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::f".into() => hir::Item {
                id: ItemId::new(0, 0),
                accessibility: ast::Accessibility::Default,
                kind: hir::ItemKind::FnDecl(
                    hir::FnDecl {
                        body: hir::Body {
                            ret_type: Some(
                                hir::Type {
                                    kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                },
                            ),
                            args: vec![
                                hir::FormalArgDef {
                                    id: FormalArgId::new(0),
                                    ref_mut: ast::RefMut::None,
                                    r#type: hir::Type {
                                        kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                    },
                                },
                            ],
                            vars: vec![
                                hir::VarDef {
                                    id: Id { id: "id".to_string(), span: Span::new(0, 0) },
                                    ref_mut: ast::RefMut::None,
                                    r#type: None,
                                    init: None,
                                },
                            ],
                            exprs: vec![
                                hir::Expr {
                                    id: ExprId::new(0),
                                    kind: hir::ExprKind::VarDef(VarId::new(0)),
                                },
                            ],
                        },
                    },
                ),
            },
        },
    };

    assert_eq!(
        jsify.jsify(&hir),
        Js {
            items: hashmap! {
                "my_hako::f".into() => Item {
                    id: ItemId::new(0, 0),
                    kind: ItemKind::FnDecl(
                        FnDecl {
                            body: Body {
                                arg_len: 1,
                                elems: vec![
                                    Elem::VarDef(
                                        VarDef {
                                            id: VarId::new(0),
                                            init: None,
                                        },
                                    ),
                                ],
                            },
                        },
                    ),
                },
            },
        },
    );
    assert!(jsify.get_logs().is_empty());
}

#[test]
fn jsifies_fn_decl_item() {
    let mut jsify = Jsify::new();
    let hir = hir::Item {
        id: ItemId::new(0, 0),
        accessibility: ast::Accessibility::Default,
        kind: hir::ItemKind::FnDecl(
            hir::FnDecl {
                body: hir::Body {
                    ret_type: Some(
                        hir::Type {
                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                        },
                    ),
                    args: vec![
                        hir::FormalArgDef {
                            id: FormalArgId::new(0),
                            ref_mut: ast::RefMut::None,
                            r#type: hir::Type {
                                kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                            },
                        },
                    ],
                    vars: vec![
                        hir::VarDef {
                            id: Id { id: "id".to_string(), span: Span::new(0, 0) },
                            ref_mut: ast::RefMut::None,
                            r#type: None,
                            init: None,
                        },
                    ],
                    exprs: vec![
                        hir::Expr {
                            id: ExprId::new(0),
                            kind: hir::ExprKind::VarDef(VarId::new(0)),
                        },
                    ],
                },
            },
        ),
    };

    assert_eq!(
        jsify.jsify_item(&hir),
        Item {
            id: ItemId::new(0, 0),
            kind: ItemKind::FnDecl(
                FnDecl {
                    body: Body {
                        arg_len: 1,
                        elems: vec![
                            Elem::VarDef(
                                VarDef {
                                    id: VarId::new(0),
                                    init: None,
                                },
                            ),
                        ],
                    },
                },
            ),
        },
    );
    assert!(jsify.get_logs().is_empty());
}

/* literal */

#[test]
fn jsifies_literal_expr() {
    let mut jsify = Jsify::new();
    let hir = hir::Expr {
        id: ExprId::new(0),
        kind: hir::ExprKind::Literal(
            token::Literal::Bool { value: true },
        ),
    };
    let vars = Vec::new();

    assert_eq!(
        jsify.jsify_expr(&hir, &vars),
        Elem::Literal(
            token::Literal::Bool { value: true },
        ),
    );
    assert!(jsify.get_logs().is_empty());
}

#[test]
fn jsifies_var_def_expr() {
    let mut jsify = Jsify::new();
    let hir = hir::Expr {
        id: ExprId::new(0),
        kind: hir::ExprKind::VarDef(VarId::new(0)),
    };
    let vars = vec![
        hir::VarDef {
            id: Id { id: "id".to_string(), span: Span::new(0, 0) },
            ref_mut: ast::RefMut::None,
            r#type: None,
            init: Some(
                hir::Expr {
                    id: ExprId::new(0),
                    kind: hir::ExprKind::Literal(
                        token::Literal::Bool { value: true },
                    ),
                },
            ),
        },
    ];

    assert_eq!(
        jsify.jsify_expr(&hir, &vars),
        Elem::VarDef(
            VarDef {
                id: VarId::new(0),
                init: Some(
                    Box::new(
                        Elem::Literal(
                            token::Literal::Bool { value: true },
                        ),
                    ),
                ),
            },
        ),
    );
    assert!(jsify.get_logs().is_empty());
}
