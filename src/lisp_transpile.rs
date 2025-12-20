use crate::ast::Stmt;
use crate::error::ParseError;
use crate::lisp::{Parser, Transformer};

/// Parse Lisp syntax and convert to internal AST
pub fn parse_lisp(input: &str) -> Result<Vec<Stmt>, ParseError> {
    // Parse S-expressions
    let mut parser = Parser::new(input);
    let sexps = parser.parse().map_err(|e| ParseError {
        message: format!("Parse error: {}", e),
        span: 0..0,
        source: input.to_string(),
    })?;

    // Transform to internal AST
    let sexps: Vec<_> = sexps.into_iter().map(|s| s.sexp).collect();
    let mut transformer = Transformer::new(input);
    let stmts = transformer
        .transform_program(sexps)
        .map_err(|e| ParseError {
            message: format!("Transform error: {}", e),
            span: 0..0,
            source: input.to_string(),
        })?;

    Ok(stmts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, ExprKind};

    #[test]
    fn test_parse_lisp_number() {
        let stmts = parse_lisp("42").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => {
                assert!(matches!(expr.kind, ExprKind::Number(42.0)));
            }
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_parse_lisp_add() {
        let stmts = parse_lisp("(+ 1 2)").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => match &expr.kind {
                ExprKind::Binary { op, .. } => {
                    assert!(matches!(op, BinOp::Add));
                }
                _ => panic!("Expected Binary expression"),
            },
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_parse_lisp_let() {
        let stmts = parse_lisp("(let [x 10] (+ x 5))").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => match &expr.kind {
                ExprKind::Block(stmts) => {
                    assert_eq!(stmts.len(), 2);
                    match &stmts[0] {
                        Stmt::Let { name, .. } => {
                            assert_eq!(name, "x");
                        }
                        _ => panic!("Expected Let statement"),
                    }
                }
                _ => panic!("Expected Block expression"),
            },
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_parse_lisp_defn() {
        let stmts = parse_lisp("(defn add [x y] (+ x y))").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Fn { name, params, .. } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
            }
            _ => panic!("Expected Fn statement"),
        }
    }
}
