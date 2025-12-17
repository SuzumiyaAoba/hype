use crate::ast::{BinOp, Expr, ExprKind, Stmt, Type};

use super::parser::Sexp;
use super::transform_type::parse_type;

pub struct Transformer {
    source: String,
}

impl Transformer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
        }
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
            Sexp::List(ref items) if items.first().map(|s| self.is_symbol(s, "defn")).unwrap_or(false) => {
                self.transform_defn(items)
            }
            // (let [bindings] body)
            Sexp::List(ref items) if items.first().map(|s| self.is_symbol(s, "let")).unwrap_or(false) => {
                Ok(Stmt::Expr(self.transform_let(items)?))
            }
            // Expression as statement
            _ => {
                let expr = self.transform_expr(sexp)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn transform_expr(&mut self, sexp: Sexp) -> Result<Expr, String> {
        let span = 0..0; // TODO: track spans properly

        let kind = match sexp {
            Sexp::Number(n) => ExprKind::Number(n),
            Sexp::String(s) => ExprKind::Str(s),
            Sexp::Bool(b) => ExprKind::Bool(b),
            Sexp::Symbol(s) => ExprKind::Var { name: s },

            // Vector becomes list literal
            Sexp::Vector(items) => {
                let exprs: Result<Vec<_>, _> = items
                    .into_iter()
                    .map(|item| self.transform_expr(item))
                    .collect();
                ExprKind::List(exprs?)
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
                    // do block
                    Sexp::Symbol(s) if s == "do" => {
                        self.transform_do(&items[1..])?
                    }
                    // fn (lambda)
                    Sexp::Symbol(s) if s == "fn" => {
                        self.transform_lambda(&items[1..])?
                    }
                    // Function call
                    Sexp::Symbol(name) => {
                        let args: Result<Vec<_>, _> = items[1..]
                            .iter()
                            .map(|arg| self.transform_expr(arg.clone()))
                            .collect();
                        ExprKind::Call {
                            callee: name.clone(),
                            callee_span: span.clone(),
                            args: args?,
                        }
                    }
                    _ => {
                        return Err(format!("Invalid expression: first element of list must be a symbol, got {:?}", items[0]));
                    }
                }
            }

            Sexp::Map(_) => {
                return Err("Map literals not yet supported".to_string());
            }
            Sexp::Tagged(_, _) => {
                return Err("Tagged expressions (HTML) not yet supported".to_string());
            }
        };

        Ok(Expr { kind, span })
    }

    fn transform_binop(&mut self, op: &str, args: &[Sexp]) -> Result<ExprKind, String> {
        if args.len() < 2 {
            return Err(format!("Binary operator '{}' requires at least 2 arguments", op));
        }

        let binop = match op {
            "+" => BinOp::Add,
            "-" => BinOp::Sub,
            "*" => BinOp::Mul,
            "/" => BinOp::Div,
            "==" => BinOp::Eq,
            "!=" => BinOp::Ne,
            "<" => BinOp::Lt,
            ">" => BinOp::Gt,
            "<=" => BinOp::Le,
            ">=" => BinOp::Ge,
            "and" => BinOp::And,
            "or" => BinOp::Or,
            _ => return Err(format!("Unknown binary operator: {}", op)),
        };

        // For now, handle exactly 2 arguments
        // TODO: support n-ary operators like (+ 1 2 3 4)
        if args.len() != 2 {
            return Err(format!("Binary operator '{}' currently only supports exactly 2 arguments", op));
        }

        let left = Box::new(self.transform_expr(args[0].clone())?);
        let right = Box::new(self.transform_expr(args[1].clone())?);

        Ok(ExprKind::Binary {
            op: binop,
            left,
            right,
        })
    }

    fn transform_let(&mut self, items: &[Sexp]) -> Result<Expr, String> {
        // (let [bindings...] body)
        if items.len() < 3 {
            return Err("let requires bindings and body".to_string());
        }

        // Parse bindings: [x 10 y 20]
        let bindings = match &items[1] {
            Sexp::Vector(binds) => binds,
            _ => return Err("let bindings must be a vector".to_string()),
        };

        if bindings.len() % 2 != 0 {
            return Err("let bindings must have even number of elements (name value pairs)".to_string());
        }

        // Transform body
        let body_sexp = if items.len() == 3 {
            items[2].clone()
        } else {
            // Multiple body expressions -> implicit do
            Sexp::List(
                std::iter::once(Sexp::Symbol("do".to_string()))
                    .chain(items[2..].iter().cloned())
                    .collect()
            )
        };

        // Build nested let expressions (for multiple bindings)
        let mut result_expr = self.transform_expr(body_sexp)?;

        // Process bindings in reverse to build nested structure
        for i in (0..bindings.len()).step_by(2).rev() {
            let name = match &bindings[i] {
                Sexp::Symbol(n) => n.clone(),
                _ => return Err("let binding name must be a symbol".to_string()),
            };

            let value = self.transform_expr(bindings[i + 1].clone())?;

            // Create a block with let statement and the current result
            result_expr = Expr {
                kind: ExprKind::Block(vec![
                    Stmt::Let {
                        name,
                        ty: None,
                        expr: value,
                        recursive: false,
                    },
                    Stmt::Expr(result_expr),
                ]),
                span: 0..0,
            };
        }

        Ok(result_expr)
    }

    fn transform_do(&mut self, items: &[Sexp]) -> Result<ExprKind, String> {
        if items.is_empty() {
            return Err("do block requires at least one expression".to_string());
        }

        let mut stmts = Vec::new();

        for (i, sexp) in items.iter().enumerate() {
            let is_last = i == items.len() - 1;

            if is_last {
                // Last expression is the return value
                let expr = self.transform_expr(sexp.clone())?;
                stmts.push(Stmt::Expr(expr));
            } else {
                // Non-last expressions become statements
                match sexp {
                    Sexp::List(list_items) if list_items.first().map(|s| self.is_symbol(s, "let")).unwrap_or(false) => {
                        // Transform let as a statement
                        let let_expr = self.transform_let(list_items)?;
                        if let ExprKind::Block(mut block_stmts) = let_expr.kind {
                            stmts.append(&mut block_stmts);
                        } else {
                            stmts.push(Stmt::Expr(let_expr));
                        }
                    }
                    _ => {
                        let expr = self.transform_expr(sexp.clone())?;
                        stmts.push(Stmt::Expr(expr));
                    }
                }
            }
        }

        Ok(ExprKind::Block(stmts))
    }

    fn transform_lambda(&mut self, items: &[Sexp]) -> Result<ExprKind, String> {
        // (fn [params] body)
        if items.len() < 2 {
            return Err("fn requires parameters and body".to_string());
        }

        let params = match &items[0] {
            Sexp::Vector(params) => {
                self.parse_params(params)?
            }
            _ => return Err("fn parameters must be a vector".to_string()),
        };

        let body = Box::new(self.transform_expr(items[1].clone())?);

        Ok(ExprKind::Lambda { params, body })
    }

    fn transform_defn(&mut self, items: &[Sexp]) -> Result<Stmt, String> {
        // (defn name [params] body)
        // または (defn name [params] -> ReturnType body)
        if items.len() < 4 {
            return Err("defn requires name, parameters, and body".to_string());
        }

        let name = match &items[1] {
            Sexp::Symbol(n) => n.clone(),
            _ => return Err("defn name must be a symbol".to_string()),
        };

        let params = match &items[2] {
            Sexp::Vector(params) => {
                self.parse_params(params)?
            }
            _ => return Err("defn parameters must be a vector".to_string()),
        };

        // Check for return type annotation: -> Type
        let (ret, body_idx) = if items.len() > 4 && matches!(&items[3], Sexp::Arrow) {
            // (defn name [params] -> RetType body)
            let ret_type = parse_type(&items[4])?;
            (Some(ret_type), 5)
        } else {
            (None, 3)
        };

        if body_idx >= items.len() {
            return Err("defn requires a body expression".to_string());
        }

        let body = self.transform_expr(items[body_idx].clone())?;

        Ok(Stmt::Fn {
            name,
            params,
            ret,
            body,
        })
    }

    fn parse_params(&self, params: &[Sexp]) -> Result<Vec<(String, Option<Type>)>, String> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < params.len() {
            match &params[i] {
                Sexp::Symbol(name) => {
                    // Check if next is a colon (type annotation)
                    if i + 2 < params.len() && matches!(&params[i + 1], Sexp::Colon) {
                        // x: Type
                        let ty = parse_type(&params[i + 2])?;
                        result.push((name.clone(), Some(ty)));
                        i += 3; // Skip name, colon, and type
                    } else {
                        // Just a name without type
                        result.push((name.clone(), None));
                        i += 1;
                    }
                }
                _ => return Err(format!("Expected parameter name, found {:?}", params[i])),
            }
        }

        Ok(result)
    }

    fn is_symbol(&self, sexp: &Sexp, name: &str) -> bool {
        matches!(sexp, Sexp::Symbol(s) if s == name)
    }

    fn is_binop(&self, op: &str) -> bool {
        matches!(op, "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "and" | "or")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
            Stmt::Expr(expr) => {
                match &expr.kind {
                    ExprKind::Binary { op, left, right } => {
                        assert!(matches!(op, BinOp::Add));
                        assert!(matches!(left.kind, ExprKind::Number(1.0)));
                        assert!(matches!(right.kind, ExprKind::Number(2.0)));
                    }
                    _ => panic!("Expected Binary expression"),
                }
            }
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_nested_arithmetic() {
        let stmts = parse_and_transform("(+ (* 2 3) 4)").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => {
                match &expr.kind {
                    ExprKind::Binary { op: BinOp::Add, left, right } => {
                        match &left.kind {
                            ExprKind::Binary { op: BinOp::Mul, .. } => {}
                            _ => panic!("Expected nested multiply"),
                        }
                        assert!(matches!(right.kind, ExprKind::Number(4.0)));
                    }
                    _ => panic!("Expected Binary expression"),
                }
            }
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_let_binding() {
        let stmts = parse_and_transform("(let [x 10] x)").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => {
                match &expr.kind {
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
                }
            }
            _ => panic!("Expected Expr statement"),
        }
    }

    #[test]
    fn test_defn() {
        let stmts = parse_and_transform("(defn add [x y] (+ x y))").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Fn { name, params, body, .. } => {
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
        let stmts = parse_and_transform("(defn add [x: Number y: Number] -> Number (+ x y))").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Fn { name, params, ret, .. } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].0, "x");
                assert!(matches!(params[0].1, Some(Type::Number)));
                assert_eq!(params[1].0, "y");
                assert!(matches!(params[1].1, Some(Type::Number)));
                assert!(matches!(ret, Some(Type::Number)));
            }
            _ => panic!("Expected Fn statement"),
        }
    }

    #[test]
    fn test_lambda_with_type_annotations() {
        let stmts = parse_and_transform("(fn [x: Number] (* x 2))").unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(expr) => {
                match &expr.kind {
                    ExprKind::Lambda { params, .. } => {
                        assert_eq!(params.len(), 1);
                        assert_eq!(params[0].0, "x");
                        assert!(matches!(params[0].1, Some(Type::Number)));
                    }
                    _ => panic!("Expected Lambda expression"),
                }
            }
            _ => panic!("Expected Expr statement"),
        }
    }
}
