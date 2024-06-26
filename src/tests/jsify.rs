use crate::*;
use crate::js::*;

use jsify::{BlockLastBind, BodyScope, StmtSeq};
use karinc::lexer::token;
use karinc::lexer::token::Span;
use karinc::parser::ast;
use karinc::hir;
use karinc::hir::id::*;
use karinc::typesys;
use karinc::typesys::constraint::TypeConstraint;
use maplit::hashmap;

fn generate_body(id: usize) -> hir::Body {
    hir::Body {
        id: BodyId::new(id),
        ret_type: None,
        args: Vec::new(),
        vars: Vec::new(),
        exprs: Vec::new(),
    }
}

#[test]
fn jsifies_literal_expr() {
    let type_table = TypeConstraintTable::new();
    let mut jsify = Jsify::new(&type_table);
    let hir = hir::Expr {
        id: ExprId::new(0),
        kind: hir::ExprKind::Literal(
            token::Literal::Bool { value: true },
        ),
    };
    let body = generate_body(0);
    let mut body_scope = BodyScope::new(&body);
    let mut stmt_seq = StmtSeq::new();
    let result = jsify.jsify_expr(&mut body_scope, &mut stmt_seq, &hir, false);

    assert_eq!(
        stmt_seq,
        Vec::new().into(),
    );
    assert_eq!(
        result,
        Stmt::Expr(
            Expr::Literal(
                Literal::Derived(
                    token::Literal::Bool { value: true },
                ),
            ),
        ),
    );
    assert!(jsify.get_logs().is_empty());
}

#[test]
fn jsifies_block_expr() {
    let type_table = TypeConstraintTable::new();
    let mut jsify = Jsify::new(&type_table);
    let hir = hir::Expr {
        id: ExprId::new(0),
        kind: hir::ExprKind::Block(
            hir::Block {
                exprs: vec![
                    hir::Expr {
                        id: ExprId::new(0),
                        kind: hir::ExprKind::Literal(
                            token::Literal::Bool { value: true },
                        ),
                    },
                ],
            },
        ),
    };
    let body = generate_body(0);
    let mut body_scope = BodyScope::new(&body);
    let mut stmt_seq = StmtSeq::new();
    let result = jsify.jsify_expr(&mut body_scope, &mut stmt_seq, &hir, false);

    assert_eq!(
        stmt_seq,
        Vec::new().into(),
    );
    assert_eq!(
        result,
        Stmt::Block(
            Block {
                stmts: vec![
                    Stmt::Expr(
                        Expr::Literal(
                            Literal::Derived(
                                token::Literal::Bool { value: true },
                            ),
                        ),
                    ),
                ],
            },
        ),
    );
    assert!(jsify.get_logs().is_empty());
}

#[test]
fn jsifies_block_with_last_bind() {
    let type_table = TypeConstraintTable::new();
    let mut jsify = Jsify::new(&type_table);
    let hir = hir::Block {
        exprs: vec![
            hir::Expr {
                id: ExprId::new(0),
                kind: hir::ExprKind::Literal(
                    token::Literal::Bool { value: true },
                ),
            },
            hir::Expr {
                id: ExprId::new(1),
                kind: hir::ExprKind::Literal(
                    token::Literal::Bool { value: true },
                ),
            },
        ],
    };
    let body = generate_body(0);
    let mut body_scope = BodyScope::new(&body);
    let result = jsify.jsify_block(&mut body_scope, &hir, BlockLastBind::LastBind { tmp_id: 0 });

    assert_eq!(
        result,
        Block {
            stmts: vec![
                Stmt::Expr(
                    Expr::Literal(
                        Literal::Derived(
                            token::Literal::Bool { value: true },
                        ),
                    ),
                ),
                Stmt::VarBind(
                    VarBind {
                        id: Id::Tmp(0),
                        value: Box::new(
                            Expr::Literal(
                                Literal::Derived(
                                    token::Literal::Bool { value: true },
                                ),
                            ),
                        ),
                    },
                ),
            ],
        },
    );
    assert!(jsify.get_logs().is_empty());
}

#[test]
fn jsifies_var_def_expr() {
    let type_table = TypeConstraintTable::new();
    let mut jsify = Jsify::new(&type_table);
    let hir = hir::Expr {
        id: ExprId::new(0),
        kind: hir::ExprKind::VarDef(VarId::new(0)),
    };
    let mut body = generate_body(0);
    body.vars = vec![
        hir::VarDef {
            id: ast::Id { id: "id".to_string(), span: Span::new(0, 0) },
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
    let mut body_scope = BodyScope::new(&body);
    let mut stmt_seq = StmtSeq::new();
    let result = jsify.jsify_expr(&mut body_scope, &mut stmt_seq, &hir, false);

    assert_eq!(
        stmt_seq,
        Vec::new().into(),
    );
    assert_eq!(
        result,
        Stmt::VarDef(
            VarDef {
                id: Id::Var(0),
                init: Some(
                    Expr::Literal(
                        Literal::Derived(
                            token::Literal::Bool { value: true },
                        ),
                    ),
                ),
            }
        ),
    );
    assert!(jsify.get_logs().is_empty());
}

#[test]
fn jsifies_var_bind_expr() {
    let type_table = TypeConstraintTable::new();
    let mut jsify = Jsify::new(&type_table);
    let hir = hir::Expr {
        id: ExprId::new(0),
        kind: hir::ExprKind::VarBind(
            hir::VarBind {
                var_id: VarId::new(0),
                value: Box::new(
                    hir::Expr {
                        id: ExprId::new(0),
                        kind: hir::ExprKind::Literal(
                            token::Literal::Bool { value: true },
                        ),
                    },
                ),
            },
        ),
    };
    let body = generate_body(0);
    let mut body_scope = BodyScope::new(&body);
    let mut stmt_seq = StmtSeq::new();
    let result = jsify.jsify_expr(&mut body_scope, &mut stmt_seq, &hir, false);

    assert_eq!(
        stmt_seq,
        Vec::new().into(),
    );
    assert_eq!(
        result,
        Stmt::VarBind(
            VarBind {
                id: Id::Var(0),
                value: Box::new(
                    Expr::Literal(
                        Literal::Derived(
                            token::Literal::Bool { value: true },
                        ),
                    ),
                ),
            }
        ),
    );
    assert!(jsify.get_logs().is_empty());
}

#[test]
fn jsifies_if_expr() {
    let type_table = hashmap! {
        TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Void)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Void)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Void)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(3)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Void)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(4)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Void)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(5)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Void)),
        ),
    }.into();
    let mut jsify = Jsify::new(&type_table);
    let hir = hir::Expr {
        id: ExprId::new(0),
        kind: hir::ExprKind::If(
            hir::If {
                cond: Box::new(
                    hir::Expr {
                        id: ExprId::new(1),
                        kind: hir::ExprKind::Literal(
                            token::Literal::Bool { value: true },
                        ),
                    },
                ),
                block: hir::Block {
                    exprs: vec![
                        hir::Expr {
                            id: ExprId::new(2),
                            kind: hir::ExprKind::Literal(
                                token::Literal::Bool { value: true },
                            ),
                        },
                    ],
                },
                elifs: vec![
                    hir::Elif {
                        cond: Box::new(
                            hir::Expr {
                                id: ExprId::new(3),
                                kind: hir::ExprKind::Literal(
                                    token::Literal::Bool { value: true },
                                ),
                            },
                        ),
                        block: hir::Block {
                            exprs: vec![
                                hir::Expr {
                                    id: ExprId::new(4),
                                    kind: hir::ExprKind::Literal(
                                        token::Literal::Bool { value: true },
                                    ),
                                },
                            ],
                        },
                    },
                ],
                r#else: Some(
                    hir::Block {
                        exprs: vec![
                            hir::Expr {
                                id: ExprId::new(5),
                                kind: hir::ExprKind::Literal(
                                    token::Literal::Bool { value: true },
                                ),
                            },
                        ],
                    },
                ),
            },
        ),
    };
    let body = generate_body(0);
    let mut body_scope = BodyScope::new(&body);
    let mut stmt_seq = StmtSeq::new();
    let result = jsify.jsify_expr(&mut body_scope, &mut stmt_seq, &hir, false);

    assert_eq!(
        stmt_seq,
        Vec::new().into(),
    );
    assert_eq!(
        result,
        Stmt::If(
            If {
                cond: Box::new(
                    Expr::Literal(
                        Literal::Derived(
                            token::Literal::Bool { value: true },
                        ),
                    ),
                ),
                block: Block {
                    stmts: vec![
                        Stmt::Expr(
                            Expr::Literal(
                                Literal::Derived(
                                    token::Literal::Bool { value: true },
                                ),
                            ),
                        ),
                    ],
                },
                elifs: vec![
                    Elif {
                        cond: Box::new(
                            Expr::Literal(
                                Literal::Derived(
                                    token::Literal::Bool { value: true },
                                ),
                            ),
                        ),
                        block: Block {
                            stmts: vec![
                                Stmt::Expr(
                                    Expr::Literal(
                                        Literal::Derived(
                                            token::Literal::Bool { value: true },
                                        ),
                                    ),
                                ),
                            ],
                        },
                    },
                ],
                r#else: Some(
                    Block {
                        stmts: vec![
                            Stmt::Expr(
                                Expr::Literal(
                                    Literal::Derived(
                                        token::Literal::Bool { value: true },
                                    ),
                                ),
                            ),
                        ],
                    },
                ),
            },
        ),
    );
    assert!(jsify.get_logs().is_empty());
}

#[test]
fn jsifies_if_expr_expected_to_be_expr() {
    let type_table = hashmap! {
        TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Bool)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Bool)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Bool)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(3)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Bool)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(4)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Bool)),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(5)) => TypeConstraint::new(
            typesys::TypePtr::new(typesys::Type::Prim(ast::PrimType::Bool)),
        ),
    }.into();
    let mut jsify = Jsify::new(&type_table);
    let hir = hir::Expr {
        id: ExprId::new(0),
        kind: hir::ExprKind::If(
            hir::If {
                cond: Box::new(
                    hir::Expr {
                        id: ExprId::new(1),
                        kind: hir::ExprKind::Literal(
                            token::Literal::Bool { value: true },
                        ),
                    },
                ),
                block: hir::Block {
                    exprs: vec![
                        hir::Expr {
                            id: ExprId::new(2),
                            kind: hir::ExprKind::Literal(
                                token::Literal::Bool { value: true },
                            ),
                        },
                    ],
                },
                elifs: vec![
                    hir::Elif {
                        cond: Box::new(
                            hir::Expr {
                                id: ExprId::new(3),
                                kind: hir::ExprKind::Literal(
                                    token::Literal::Bool { value: true },
                                ),
                            },
                        ),
                        block: hir::Block {
                            exprs: vec![
                                hir::Expr {
                                    id: ExprId::new(4),
                                    kind: hir::ExprKind::Literal(
                                        token::Literal::Bool { value: true },
                                    ),
                                },
                            ],
                        },
                    },
                ],
                r#else: Some(
                    hir::Block {
                        exprs: vec![
                            hir::Expr {
                                id: ExprId::new(5),
                                kind: hir::ExprKind::Literal(
                                    token::Literal::Bool { value: true },
                                ),
                            },
                        ],
                    },
                ),
            },
        ),
    };
    let body = generate_body(0);
    let mut body_scope = BodyScope::new(&body);
    let mut stmt_seq = StmtSeq::new();
    let result = jsify.jsify_expr(&mut body_scope, &mut stmt_seq, &hir, true);

    assert_eq!(
        stmt_seq,
        vec![
            Stmt::VarDef(
                VarDef {
                    id: Id::Tmp(0),
                    init: None,
                },
            ),
            Stmt::If(
                If {
                    cond: Box::new(
                        Expr::Literal(
                            Literal::Derived(
                                token::Literal::Bool { value: true },
                            ),
                        ),
                    ),
                    block: Block {
                        stmts: vec![
                            Stmt::VarBind(
                                VarBind {
                                    id: Id::Tmp(0),
                                    value: Box::new(
                                        Expr::Literal(
                                            Literal::Derived(
                                                token::Literal::Bool { value: true },
                                            ),
                                        ),
                                    ),
                                },
                            ),
                        ],
                    },
                    elifs: vec![
                        Elif {
                            cond: Box::new(
                                Expr::Literal(
                                    Literal::Derived(
                                        token::Literal::Bool { value: true },
                                    ),
                                ),
                            ),
                            block: Block {
                                stmts: vec![
                                    Stmt::VarBind(
                                        VarBind {
                                            id: Id::Tmp(0),
                                            value: Box::new(
                                                Expr::Literal(
                                                    Literal::Derived(
                                                        token::Literal::Bool { value: true },
                                                    ),
                                                ),
                                            ),
                                        },
                                    ),
                                ],
                            },
                        },
                    ],
                    r#else: Some(
                        Block {
                            stmts: vec![
                                Stmt::VarBind(
                                    VarBind {
                                        id: Id::Tmp(0),
                                        value: Box::new(
                                            Expr::Literal(
                                                Literal::Derived(
                                                    token::Literal::Bool { value: true },
                                                ),
                                            ),
                                        ),
                                    },
                                ),
                            ],
                        },
                    ),
                },
            ),
        ].into(),
    );
    assert_eq!(
        result,
        Stmt::Expr(Expr::Id(Id::Tmp(0))),
    );
    assert!(jsify.get_logs().is_empty());
}
