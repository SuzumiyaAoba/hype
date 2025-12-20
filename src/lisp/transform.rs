mod expr_transform;
mod pattern_transform;
mod stmt_transform;

use crate::ast::{Expr, ExprKind, Stmt};

use super::parser::Sexp;

pub struct Transformer;

impl Transformer {
    pub fn new(_source: &str) -> Self {
        Self
    }

    pub fn transform_program(&mut self, sexps: Vec<Sexp>) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();
        for sexp in sexps {
            stmts.push(self.transform_stmt(sexp)?);
        }
        Ok(stmts)
    }

    fn transform_stmt(&mut self, sexp: Sexp) -> Result<Stmt, String> {
        match sexp {
            // (defn name [params] body)
            Sexp::List(ref items)
                if items
                    .first()
                    .map(|s| self.is_symbol(s, "defn"))
                    .unwrap_or(false) =>
            {
                self.transform_defn(items)
            }
            // (deftype Name [params] Variant1 (Variant2 {:field Type}) ...)
            Sexp::List(ref items)
                if items
                    .first()
                    .map(|s| self.is_symbol(s, "deftype"))
                    .unwrap_or(false) =>
            {
                self.transform_deftype(items)
            }
            // (import "path")
            Sexp::List(ref items)
                if items
                    .first()
                    .map(|s| self.is_symbol(s, "import"))
                    .unwrap_or(false) =>
            {
                self.transform_import(items)
            }
            // (external name Type "js_name")
            Sexp::List(ref items)
                if items
                    .first()
                    .map(|s| self.is_symbol(s, "external"))
                    .unwrap_or(false) =>
            {
                self.transform_external(items)
            }
            // (let [bindings] body)
            Sexp::List(ref items)
                if items
                    .first()
                    .map(|s| self.is_symbol(s, "let"))
                    .unwrap_or(false) =>
            {
                Ok(Stmt::Expr(self.transform_let(items)?))
            }
            // (let-rec [bindings] body)
            Sexp::List(ref items)
                if items
                    .first()
                    .map(|s| self.is_symbol(s, "let-rec"))
                    .unwrap_or(false) =>
            {
                Ok(Stmt::Expr(self.transform_let_rec(items)?))
            }
            // Expression as statement
            _ => {
                let expr = self.transform_expr(sexp)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    pub(crate) fn transform_expr(&mut self, sexp: Sexp) -> Result<Expr, String> {
        let span = 0..0; // TODO: track spans properly

        let kind = match sexp {
            Sexp::Number(n) => ExprKind::Number(n),
            Sexp::String(s) => ExprKind::Str(s),
            Sexp::Bool(b) => ExprKind::Bool(b),
            Sexp::Symbol(s) => {
                // Uppercase symbols are constructors without fields
                let is_upper = s.chars().next().is_some_and(|c| c.is_ascii_uppercase());
                if is_upper {
                    ExprKind::Constructor {
                        name: s,
                        fields: Vec::new(),
                    }
                } else {
                    ExprKind::Var { name: s }
                }
            }

            // Vector becomes list literal
            Sexp::Vector(items) => {
                let exprs: Result<Vec<_>, _> = items
                    .into_iter()
                    .map(|item| self.transform_expr(item))
                    .collect();
                ExprKind::List(exprs?)
            }

            // Tuple (comma-separated vector)
            Sexp::Tuple(items) => {
                let exprs: Result<Vec<_>, _> = items
                    .into_iter()
                    .map(|item| self.transform_expr(item))
                    .collect();
                ExprKind::Tuple(exprs?)
            }

            // List: function call or special form
            Sexp::List(items) => {
                if items.is_empty() {
                    return Err("Empty list is not a valid expression".to_string());
                }

                match &items[0] {
                    // Arithmetic operators
                    Sexp::Symbol(op) if self.is_binop(op) => {
                        self.transform_binop(op, &items[1..])?
                    }
                    // let expression
                    Sexp::Symbol(s) if s == "let" => {
                        return Ok(self.transform_let(&items)?);
                    }
                    // let-rec expression
                    Sexp::Symbol(s) if s == "let-rec" => {
                        return Ok(self.transform_let_rec(&items)?);
                    }
                    // do block
                    Sexp::Symbol(s) if s == "do" => self.transform_do(&items[1..])?,
                    // fn (lambda)
                    Sexp::Symbol(s) if s == "fn" => self.transform_lambda(&items[1..])?,
                    // match expression
                    Sexp::Symbol(s) if s == "match" => self.transform_match(&items[1..])?,
                    // if expression
                    Sexp::Symbol(s) if s == "if" => self.transform_if(&items[1..])?,
                    // Field access: (.field expr)
                    Sexp::Symbol(s) if s.starts_with('.') && s.len() > 1 => {
                        if items.len() != 2 {
                            return Err("field access requires exactly one argument".to_string());
                        }
                        let field = s[1..].to_string();
                        let record_expr = Box::new(self.transform_expr(items[1].clone())?);
                        ExprKind::FieldAccess {
                            expr: record_expr,
                            field,
                        }
                    }
                    // Constructor or Function call
                    Sexp::Symbol(name) => {
                        let is_upper = name.chars().next().is_some_and(|c| c.is_ascii_uppercase());
                        if is_upper {
                            // Constructor with fields: (Name {:field expr})
                            if items.len() > 1 {
                                match &items[1] {
                                    Sexp::Map(pairs) => {
                                        let mut fields = Vec::new();
                                        for (key, value) in pairs {
                                            let field_name =
                                                match key {
                                                    Sexp::Symbol(n) => n.clone(),
                                                    _ => return Err(
                                                        "Constructor field name must be a symbol"
                                                            .to_string(),
                                                    ),
                                                };
                                            let field_expr = self.transform_expr(value.clone())?;
                                            fields.push((field_name, field_expr));
                                        }
                                        ExprKind::Constructor {
                                            name: name.clone(),
                                            fields,
                                        }
                                    }
                                    _ => {
                                        // Treat as function call for now
                                        let args: Result<Vec<_>, _> = items[1..]
                                            .iter()
                                            .map(|arg| self.transform_expr(arg.clone()))
                                            .collect();
                                        ExprKind::Call {
                                            callee: Box::new(Expr {
                                                kind: ExprKind::Constructor {
                                                    name: name.clone(),
                                                    fields: Vec::new(),
                                                },
                                                span: span.clone(),
                                            }),
                                            args: args?,
                                        }
                                    }
                                }
                            } else {
                                // Constructor without fields: (Name)
                                ExprKind::Constructor {
                                    name: name.clone(),
                                    fields: Vec::new(),
                                }
                            }
                        } else {
                            // Function call
                            let args: Result<Vec<_>, _> = items[1..]
                                .iter()
                                .map(|arg| self.transform_expr(arg.clone()))
                                .collect();
                            ExprKind::Call {
                                callee: Box::new(Expr {
                                    kind: ExprKind::Var { name: name.clone() },
                                    span: span.clone(),
                                }),
                                args: args?,
                            }
                        }
                    }
                    // Expression call: ((fn [x] ...) arg) or ((make_adder 5) 10)
                    Sexp::List(_) => {
                        let callee = Box::new(self.transform_expr(items[0].clone())?);
                        let args: Result<Vec<_>, _> = items[1..]
                            .iter()
                            .map(|arg| self.transform_expr(arg.clone()))
                            .collect();
                        ExprKind::Call {
                            callee,
                            args: args?,
                        }
                    }
                    _ => {
                        return Err(format!(
                            "Invalid expression: first element of list must be a symbol or expression, got {:?}",
                            items[0]
                        ));
                    }
                }
            }

            Sexp::Map(pairs) => {
                // Record literal: {:x 1 :y 2}
                let mut fields = Vec::new();
                for (key, value) in pairs {
                    let field_name = match key {
                        Sexp::Symbol(n) => n.clone(),
                        _ => return Err("Record field name must be a symbol".to_string()),
                    };
                    let field_expr = self.transform_expr(value.clone())?;
                    fields.push((field_name, field_expr));
                }
                ExprKind::Record(fields)
            }
            Sexp::Tagged(_, _) => {
                return Err("Tagged expressions (HTML) not yet supported".to_string());
            }
            Sexp::Colon => {
                return Err("Colon (:) is not a valid expression".to_string());
            }
            Sexp::Arrow => {
                return Err("Arrow (->) is not a valid expression".to_string());
            }
            Sexp::Ellipsis => {
                return Err("Ellipsis (...) is not a valid expression".to_string());
            }
            Sexp::Keyword(kw) => {
                return Err(format!("Keyword :{} is not a valid expression", kw));
            }
        };

        Ok(Expr { kind, span })
    }

    pub(crate) fn is_symbol(&self, sexp: &Sexp, name: &str) -> bool {
        matches!(sexp, Sexp::Symbol(s) if s == name)
    }

    pub(crate) fn is_binop(&self, op: &str) -> bool {
        matches!(
            op,
            "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "and" | "or"
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::BinOp;
    use crate::lisp::parser::Parser;

    fn parse_and_transform(input: &str) -> Result<Vec<Stmt>, String> {
        let mut parser = Parser::new(input);
        let sexps = parser.parse()?;
        let sexps: Vec<Sexp> = sexps.into_iter().map(|s| s.sexp).collect();
        let mut transformer = Transformer::new(input);
        transformer.transform_program(sexps)
    }

    #[test]
    fn test_number() {
        let stmts = parse_and_transform("42").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => {
                assert!(matches!(expr.kind, ExprKind::Number(42.0)));
            }
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_simple_add() {
        let stmts = parse_and_transform("(+ 1 2)").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => match &expr.kind {
                ExprKind::Binary { op, left, right } => {
                    assert!(matches!(op, BinOp::Add));
                    assert!(matches!(left.kind, ExprKind::Number(1.0)));
                    assert!(matches!(right.kind, ExprKind::Number(2.0)));
                }
                _ => panic!("Expected Binary expression"),
            },
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_nested_arithmetic() {
        let stmts = parse_and_transform("(+ (* 2 3) 4)").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => match &expr.kind {
                ExprKind::Binary {
                    op: BinOp::Add,
                    left,
                    right,
                } => {
                    match &left.kind {
                        ExprKind::Binary { op: BinOp::Mul, .. } => {}
                        _ => panic!("Expected nested multiply"),
                    }
                    assert!(matches!(right.kind, ExprKind::Number(4.0)));
                }
                _ => panic!("Expected Binary expression"),
            },
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_let_binding() {
        let stmts = parse_and_transform("(let [x 10] x)").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => match &expr.kind {
                ExprKind::Block(stmts) => {
                    assert_eq!(stmts.len(), 2);
                    match &stmts[0] {
                        Stmt::Let { name, expr, .. } => {
                            assert_eq!(name, "x");
                            assert!(matches!(expr.kind, ExprKind::Number(10.0)));
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
    fn test_defn() {
        let stmts = parse_and_transform("(defn add [x y] (+ x y))").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Fn {
                name, params, body, ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].0, "x");
                assert_eq!(params[1].0, "y");
                match &body.kind {
                    ExprKind::Binary { op: BinOp::Add, .. } => {}
                    _ => panic!("Expected Binary expression in body"),
                }
            }
            _ => panic!("Expected Fn statement"),
        }
    }

    #[test]
    fn test_defn_with_type_annotations() {
        let stmts =
            parse_and_transform("(defn add [x: Number y: Number] -> Number (+ x y))").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Fn {
                name, params, ret, ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].0, "x");
                assert!(matches!(params[0].1, Some(crate::ast::Type::Number)));
                assert_eq!(params[1].0, "y");
                assert!(matches!(params[1].1, Some(crate::ast::Type::Number)));
                assert!(matches!(ret, Some(crate::ast::Type::Number)));
            }
            _ => panic!("Expected Fn statement"),
        }
    }

    #[test]
    fn test_lambda_with_type_annotations() {
        let stmts = parse_and_transform("(fn [x: Number] (* x 2))").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => match &expr.kind {
                ExprKind::Lambda { params, .. } => {
                    assert_eq!(params.len(), 1);
                    assert_eq!(params[0].0, "x");
                    assert!(matches!(params[0].1, Some(crate::ast::Type::Number)));
                }
                _ => panic!("Expected Lambda expression"),
            },
            _ => panic!("Expected Expr statement"),
        }
    }
}
