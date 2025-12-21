use crate::ast::{BinOp, Expr, ExprKind, MatchArm, Pattern, Stmt};

use super::super::parser::Sexp;
use super::Transformer;

impl Transformer {
    pub(super) fn transform_binop(&mut self, op: &str, args: &[Sexp]) -> Result<ExprKind, String> {
        // Handle unary minus: (- x) => (0 - x)
        if op == "-" && args.len() == 1 {
            let zero = Box::new(Expr {
                kind: ExprKind::Number(0.0),
                span: 0..0,
            });
            let right = Box::new(self.transform_expr(args[0].clone())?);
            return Ok(ExprKind::Binary {
                op: BinOp::Sub,
                left: zero,
                right,
            });
        }

        if args.len() < 2 {
            return Err(format!(
                "Binary operator '{}' requires at least 2 arguments",
                op
            ));
        }

        let binop = match op {
            "+" => BinOp::Add,
            "-" => BinOp::Sub,
            "*" => BinOp::Mul,
            "/" => BinOp::Div,
            "%" => BinOp::Mod,
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
            return Err(format!(
                "Binary operator '{}' currently only supports exactly 2 arguments",
                op
            ));
        }

        let left = Box::new(self.transform_expr(args[0].clone())?);
        let right = Box::new(self.transform_expr(args[1].clone())?);

        Ok(ExprKind::Binary {
            op: binop,
            left,
            right,
        })
    }

    pub(super) fn transform_let(&mut self, items: &[Sexp]) -> Result<Expr, String> {
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
            return Err(
                "let bindings must have even number of elements (name value pairs)".to_string(),
            );
        }

        // Transform body
        let body_sexp = if items.len() == 3 {
            items[2].clone()
        } else {
            // Multiple body expressions -> implicit do
            Sexp::List(
                std::iter::once(Sexp::Symbol("do".to_string()))
                    .chain(items[2..].iter().cloned())
                    .collect(),
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

    pub(super) fn transform_do(&mut self, items: &[Sexp]) -> Result<ExprKind, String> {
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
                    Sexp::List(list_items)
                        if list_items
                            .first()
                            .map(|s| self.is_symbol(s, "let"))
                            .unwrap_or(false) =>
                    {
                        // Transform let as a statement
                        let let_expr = self.transform_let(list_items)?;
                        if let ExprKind::Block(mut block_stmts) = let_expr.kind {
                            stmts.append(&mut block_stmts);
                        } else {
                            stmts.push(Stmt::Expr(let_expr));
                        }
                    }
                    Sexp::List(list_items)
                        if list_items
                            .first()
                            .map(|s| self.is_symbol(s, "defn"))
                            .unwrap_or(false) =>
                    {
                        // Transform defn as a statement in a do block
                        let fn_stmt = self.transform_defn(list_items)?;
                        stmts.push(fn_stmt);
                    }
                    Sexp::List(list_items)
                        if list_items
                            .first()
                            .map(|s| self.is_symbol(s, "let-rec"))
                            .unwrap_or(false) =>
                    {
                        // Transform let-rec as a statement in a do block
                        let let_rec_expr = self.transform_let_rec(list_items)?;
                        if let ExprKind::Block(mut block_stmts) = let_rec_expr.kind {
                            stmts.append(&mut block_stmts);
                        } else {
                            stmts.push(Stmt::Expr(let_rec_expr));
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

    pub(super) fn transform_lambda(&mut self, items: &[Sexp]) -> Result<ExprKind, String> {
        // (fn [params] body)
        if items.len() < 2 {
            return Err("fn requires parameters and body".to_string());
        }

        let params = match &items[0] {
            Sexp::Vector(params) => self.parse_params(params)?,
            _ => return Err("fn parameters must be a vector".to_string()),
        };

        let body = Box::new(self.transform_expr(items[1].clone())?);

        Ok(ExprKind::Lambda { params, body })
    }

    pub(super) fn transform_let_rec(&mut self, items: &[Sexp]) -> Result<Expr, String> {
        // (let-rec [name value] body)
        if items.len() < 3 {
            return Err("let-rec requires bindings and body".to_string());
        }

        // Parse bindings: [name value]
        let bindings = match &items[1] {
            Sexp::Vector(binds) | Sexp::Tuple(binds) => binds,
            _ => return Err("let-rec bindings must be a vector".to_string()),
        };

        if bindings.len() != 2 {
            return Err("let-rec currently supports exactly one binding [name value]".to_string());
        }

        let name = match &bindings[0] {
            Sexp::Symbol(n) => n.clone(),
            _ => return Err("let-rec binding name must be a symbol".to_string()),
        };

        let value = self.transform_expr(bindings[1].clone())?;

        // Transform body
        let body_sexp = if items.len() == 3 {
            items[2].clone()
        } else {
            // Multiple body expressions -> implicit do
            Sexp::List(
                std::iter::once(Sexp::Symbol("do".to_string()))
                    .chain(items[2..].iter().cloned())
                    .collect(),
            )
        };

        let body = self.transform_expr(body_sexp)?;

        // Create block with recursive let
        Ok(Expr {
            kind: ExprKind::Block(vec![
                Stmt::Let {
                    name,
                    ty: None,
                    expr: value,
                    recursive: true,
                },
                Stmt::Expr(body),
            ]),
            span: 0..0,
        })
    }

    pub(super) fn transform_match(&mut self, items: &[Sexp]) -> Result<ExprKind, String> {
        // (match expr [pat1 body1] [pat2 :when guard2 body2] ...)
        if items.is_empty() {
            return Err("match requires an expression and at least one arm".to_string());
        }

        let expr = Box::new(self.transform_expr(items[0].clone())?);
        let mut arms = Vec::new();

        for arm_sexp in &items[1..] {
            match arm_sexp {
                Sexp::Vector(arm_items) | Sexp::Tuple(arm_items) => {
                    let arm = self.parse_match_arm(arm_items)?;
                    arms.push(arm);
                }
                _ => return Err("match arm must be a vector [pattern body]".to_string()),
            }
        }

        if arms.is_empty() {
            return Err("match requires at least one arm".to_string());
        }

        Ok(ExprKind::Match { expr, arms })
    }

    pub(super) fn transform_if(&mut self, items: &[Sexp]) -> Result<ExprKind, String> {
        // (if cond then else)
        if items.len() != 3 {
            return Err(
                "if requires exactly 3 arguments: condition, then-branch, else-branch".to_string(),
            );
        }

        let cond = self.transform_expr(items[0].clone())?;
        let then_branch = self.transform_expr(items[1].clone())?;
        let else_branch = self.transform_expr(items[2].clone())?;

        // Desugar to: match (cond) { case true => then; case false => else }
        Ok(ExprKind::Match {
            expr: Box::new(cond),
            arms: vec![
                MatchArm {
                    pat: Pattern::Bool(true),
                    guard: None,
                    expr: then_branch,
                },
                MatchArm {
                    pat: Pattern::Bool(false),
                    guard: None,
                    expr: else_branch,
                },
            ],
        })
    }
}
