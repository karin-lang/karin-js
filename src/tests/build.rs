use karinc::lexer::token;

use crate::js::*;
use crate::code::build::CodeBuilder;

#[test]
fn codes_bool_expr() {
    let js = Expr::Literal(
        Literal::Derived(
            token::Literal::Bool { value: true },
        ),
    );
    let mut builder = CodeBuilder;

    assert_eq!(
        builder.code_expr(&js),
        "true".to_string(),
    );
}
