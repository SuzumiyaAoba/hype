use crate::ast::{BinOp, Expr, ExprKind, MatchArm, Pattern, Stmt, Type, Variant};

use super::parser::Sexp;
use super::transform_type::parse_type;

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
            Sexp::List(ref items) if items.first().map(|s| self.is_symbol(s, "defn")).unwrap_or(false) => {
                self.transform_defn(items)
            }
            // (deftype Name [params] Variant1 (Variant2 {:field Type}) ...)
            Sexp::List(ref items) if items.first().map(|s| self.is_symbol(s, "deftype")).unwrap_or(false) => {
                self.transform_deftype(items)
            }
            // (import "path")
            Sexp::List(ref items) if items.first().map(|s| self.is_symbol(s, "import")).unwrap_or(false) => {
                self.transform_import(items)
            }
            // (external name Type "js_name")
            Sexp::List(ref items) if items.first().map(|s| self.is_symbol(s, "external")).unwrap_or(false) => {
                self.transform_external(items)
            }
            // (let [bindings] body)
            Sexp::List(ref items) if items.first().map(|s| self.is_symbol(s, "let")).unwrap_or(false) => {
                Ok(Stmt::Expr(self.transform_let(items)?))
            }
            // (let-rec [bindings] body)
            Sexp::List(ref items) if items.first().map(|s| self.is_symbol(s, "let-rec")).unwrap_or(false) => {
                Ok(Stmt::Expr(self.transform_let_rec(items)?))
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
                    Sexp::Symbol(s) if s == "do" => {
                        self.transform_do(&items[1..])?
                    }
                    // fn (lambda)
                    Sexp::Symbol(s) if s == "fn" => {
                        self.transform_lambda(&items[1..])?
                    }
                    // match expression
                    Sexp::Symbol(s) if s == "match" => {
                        self.transform_match(&items[1..])?
                    }
                    // if expression
                    Sexp::Symbol(s) if s == "if" => {
                        self.transform_if(&items[1..])?
                    }
                    // Field access: (.field expr)
                    Sexp::Symbol(s) if s.starts_with('.') && s.len() > 1 => {
                        if items.len() != 2 {
                            return Err("field access requires exactly one argument".to_string());
                        }
                        let field = s[1..].to_string();
                        let record_expr = Box::new(self.transform_expr(items[1].clone())?);
                        ExprKind::FieldAccess { expr: record_expr, field }
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
                                            let field_name = match key {
                                                Sexp::Symbol(n) => n.clone(),
                                                _ => return Err("Constructor field name must be a symbol".to_string()),
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
                        return Err(format!("Invalid expression: first element of list must be a symbol or expression, got {:?}", items[0]));
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

    fn transform_binop(&mut self, op: &str, args: &[Sexp]) -> Result<ExprKind, String> {
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
                    Sexp::List(list_items) if list_items.first().map(|s| self.is_symbol(s, "defn")).unwrap_or(false) => {
                        // Transform defn as a statement in a do block
                        let fn_stmt = self.transform_defn(list_items)?;
                        stmts.push(fn_stmt);
                    }
                    Sexp::List(list_items) if list_items.first().map(|s| self.is_symbol(s, "let-rec")).unwrap_or(false) => {
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

    fn transform_deftype(&mut self, items: &[Sexp]) -> Result<Stmt, String> {
        // (deftype Name [A B] Variant1 (Variant2 {:field Type}) ...)
        // or (deftype Name Variant1 Variant2 ...) without type params
        if items.len() < 3 {
            return Err("deftype requires name and at least one variant".to_string());
        }

        let name = match &items[1] {
            Sexp::Symbol(n) => n.clone(),
            _ => return Err("deftype name must be a symbol".to_string()),
        };

        // Check if items[2] is a type parameter vector or a variant
        let (params, variant_start) = match &items[2] {
            Sexp::Vector(ps) | Sexp::Tuple(ps) => {
                // Type parameters: [A B ...]
                let mut result = Vec::new();
                for p in ps {
                    match p {
                        Sexp::Symbol(s) => result.push(s.clone()),
                        _ => return Err("Type parameter must be a symbol".to_string()),
                    }
                }
                (result, 3)
            }
            // Not a vector - this must be a variant, so no type parameters
            _ => (Vec::new(), 2),
        };

        // Parse variants
        let mut variants = Vec::new();
        for variant_sexp in &items[variant_start..] {
            let variant = self.parse_variant(variant_sexp)?;
            variants.push(variant);
        }

        if variants.is_empty() {
            return Err("deftype requires at least one variant".to_string());
        }

        Ok(Stmt::TypeDecl { name, params, variants })
    }

    fn parse_variant(&self, sexp: &Sexp) -> Result<Variant, String> {
        match sexp {
            // Simple variant without fields: VariantName
            Sexp::Symbol(name) => Ok(Variant {
                name: name.clone(),
                fields: Vec::new(),
            }),
            // Variant with fields: (VariantName {:field Type ...})
            Sexp::List(items) => {
                if items.is_empty() {
                    return Err("Empty variant definition".to_string());
                }

                let name = match &items[0] {
                    Sexp::Symbol(n) => n.clone(),
                    _ => return Err("Variant name must be a symbol".to_string()),
                };

                let fields = if items.len() > 1 {
                    match &items[1] {
                        Sexp::Map(pairs) => {
                            let mut result = Vec::new();
                            for (key, value) in pairs {
                                let field_name = match key {
                                    Sexp::Symbol(n) => n.clone(),
                                    _ => return Err("Variant field name must be a symbol".to_string()),
                                };
                                let field_type = parse_type(value)?;
                                result.push((field_name, field_type));
                            }
                            result
                        }
                        _ => return Err("Variant fields must be a map {:field Type}".to_string()),
                    }
                } else {
                    Vec::new()
                };

                Ok(Variant { name, fields })
            }
            _ => Err(format!("Invalid variant: {:?}", sexp)),
        }
    }

    fn transform_import(&mut self, items: &[Sexp]) -> Result<Stmt, String> {
        // (import "path")
        if items.len() != 2 {
            return Err("import requires exactly one path argument".to_string());
        }

        let path = match &items[1] {
            Sexp::String(p) => p.clone(),
            _ => return Err("import path must be a string".to_string()),
        };

        Ok(Stmt::Import { path })
    }

    fn transform_external(&mut self, items: &[Sexp]) -> Result<Stmt, String> {
        // (external name Type "js_name")
        if items.len() != 4 {
            return Err("external requires name, type, and js_name".to_string());
        }

        let name = match &items[1] {
            Sexp::Symbol(n) => n.clone(),
            _ => return Err("external name must be a symbol".to_string()),
        };

        let ty = parse_type(&items[2])?;

        let js_name = match &items[3] {
            Sexp::String(n) => n.clone(),
            _ => return Err("external js_name must be a string".to_string()),
        };

        Ok(Stmt::External { name, ty, js_name })
    }

    fn transform_let_rec(&mut self, items: &[Sexp]) -> Result<Expr, String> {
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
                    .collect()
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

    fn transform_match(&mut self, items: &[Sexp]) -> Result<ExprKind, String> {
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

    fn parse_match_arm(&mut self, items: &[Sexp]) -> Result<MatchArm, String> {
        // [pattern body] or [pattern :when guard body]
        match items.len() {
            2 => {
                // No guard: [pattern body]
                let pat = self.parse_pattern(&items[0])?;
                let body = self.transform_expr(items[1].clone())?;
                Ok(MatchArm { pat, guard: None, expr: body })
            }
            4 => {
                // With guard: [pattern :when guard body]
                let pat = self.parse_pattern(&items[0])?;

                // Check for :when keyword
                match &items[1] {
                    Sexp::Keyword(kw) if kw == "when" => {}
                    _ => return Err("expected :when keyword in guarded match arm".to_string()),
                }

                let guard = self.transform_expr(items[2].clone())?;
                let body = self.transform_expr(items[3].clone())?;
                Ok(MatchArm { pat, guard: Some(guard), expr: body })
            }
            _ => Err("match arm must be [pattern body] or [pattern :when guard body]".to_string()),
        }
    }

    fn transform_if(&mut self, items: &[Sexp]) -> Result<ExprKind, String> {
        // (if cond then else)
        if items.len() != 3 {
            return Err("if requires exactly 3 arguments: condition, then-branch, else-branch".to_string());
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

    fn parse_pattern(&self, sexp: &Sexp) -> Result<Pattern, String> {
        match sexp {
            // Wildcard pattern
            Sexp::Symbol(s) if s == "_" => Ok(Pattern::Wildcard),

            // Boolean pattern
            Sexp::Bool(b) => Ok(Pattern::Bool(*b)),

            // Number pattern
            Sexp::Number(n) => Ok(Pattern::Number(*n)),

            // String pattern
            Sexp::String(s) => Ok(Pattern::Str(s.clone())),

            // Symbol: either binding or constructor (uppercase)
            Sexp::Symbol(name) => {
                let is_upper = name.chars().next().is_some_and(|c| c.is_ascii_uppercase());
                if is_upper {
                    // Constructor without fields
                    Ok(Pattern::Constructor {
                        name: name.clone(),
                        fields: Vec::new(),
                    })
                } else {
                    // Variable binding
                    Ok(Pattern::Bind(name.clone()))
                }
            }

            // Tuple pattern: [a, b, c] (comma-separated)
            Sexp::Tuple(items) => {
                let patterns: Result<Vec<_>, _> = items.iter().map(|item| self.parse_pattern(item)).collect();
                Ok(Pattern::Tuple(patterns?))
            }

            // List pattern: [a b ...rest] (space-separated)
            Sexp::Vector(items) => {
                let mut patterns = Vec::new();
                let mut rest = None;

                for (i, item) in items.iter().enumerate() {
                    // Check for rest pattern: ...name
                    if let Sexp::Ellipsis = item {
                        // Next element should be the rest variable name
                        if i + 1 < items.len() {
                            if let Sexp::Symbol(name) = &items[i + 1] {
                                rest = Some(name.clone());
                            } else {
                                rest = Some(String::new());
                            }
                        } else {
                            rest = Some(String::new());
                        }
                        break;
                    }
                    patterns.push(self.parse_pattern(item)?);
                }

                Ok(Pattern::List { items: patterns, rest })
            }

            // Constructor pattern: (Name {:field pat ...}) or (Name)
            Sexp::List(items) => {
                if items.is_empty() {
                    return Err("Empty list is not a valid pattern".to_string());
                }

                match &items[0] {
                    Sexp::Symbol(name) if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
                        // Constructor pattern
                        let mut fields = Vec::new();

                        if items.len() > 1 {
                            // Parse fields from map: {:field pat ...}
                            match &items[1] {
                                Sexp::Map(pairs) => {
                                    for (key, value) in pairs {
                                        let field_name = match key {
                                            Sexp::Symbol(n) => n.clone(),
                                            _ => return Err("Constructor field name must be a symbol".to_string()),
                                        };
                                        let field_pat = self.parse_pattern(value)?;
                                        fields.push((field_name, field_pat));
                                    }
                                }
                                _ => return Err("Constructor fields must be a map {:field pat}".to_string()),
                            }
                        }

                        Ok(Pattern::Constructor {
                            name: name.clone(),
                            fields,
                        })
                    }
                    _ => Err("Invalid pattern: list must start with uppercase constructor name".to_string()),
                }
            }

            // Record pattern: {:field pat ...}
            Sexp::Map(pairs) => {
                let mut fields = Vec::new();
                for (key, value) in pairs {
                    let field_name = match key {
                        Sexp::Symbol(n) => n.clone(),
                        _ => return Err("Record field name must be a symbol".to_string()),
                    };
                    let field_pat = self.parse_pattern(value)?;
                    fields.push((field_name, field_pat));
                }
                Ok(Pattern::Record { fields })
            }

            _ => Err(format!("Invalid pattern: {:?}", sexp)),
        }
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
