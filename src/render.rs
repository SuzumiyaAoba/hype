use crate::ast::{BinOp, Expr, ExprKind, MatchArm, Stmt, Type};
use crate::typecheck::type_name;

pub fn binop_precedence(op: &BinOp) -> u8 {
    match op {
        BinOp::Or => 1,
        BinOp::And => 2,
        BinOp::Eq | BinOp::Ne => 3,
        BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => 4,
        BinOp::Add | BinOp::Sub => 5,
        BinOp::Mul | BinOp::Div => 6,
    }
}

fn binop_symbol(op: &BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Eq => "==",
        BinOp::Ne => "!=",
        BinOp::Lt => "<",
        BinOp::Gt => ">",
        BinOp::Le => "<=",
        BinOp::Ge => ">=",
        BinOp::And => "&&",
        BinOp::Or => "||",
    }
}

pub fn render_program(stmts: &[Stmt]) -> String {
    let mut lines = Vec::new();
    if needs_fix_prelude(stmts) {
        lines.push(FIX_JS.to_string());
    }
    for (i, stmt) in stmts.iter().enumerate() {
        match stmt {
            Stmt::Let { name, expr, .. } => {
                let js = render_js(expr, 0);
                lines.push(format!("let {name} = {js};"));
            }
            Stmt::Fn { name, params, body, .. } => {
                let js_body = render_js(body, 0);
                let param_list: Vec<String> = params.iter().map(|(p, _)| p.clone()).collect();
                lines.push(format!(
                    "function {name}({}) {{ return {js_body}; }}",
                    param_list.join(", ")
                ));
            }
            Stmt::External { name, js_name, .. } => {
                lines.push(format!("const {name} = {js_name};"));
            }
            Stmt::Import { .. } => {
                // Import statements are resolved before rendering
            }
            Stmt::Expr(expr) => {
                let js = render_js(expr, 0);
                if i == stmts.len() - 1 {
                    lines.push(js);
                } else {
                    lines.push(format!("{js};"));
                }
            }
        }
    }
    lines.join("\n")
}

const FIX_JS: &str = "function fix(f) { return (function(x) { return f(function(v) { return x(x)(v); }); })(function(x) { return f(function(v) { return x(x)(v); }); }); }";

pub fn render_js(expr: &Expr, parent_prec: u8) -> String {
    match &expr.kind {
        ExprKind::Number(n) => {
            render_number_literal(*n)
        }
        ExprKind::Bool(b) => format!("{b}"),
        ExprKind::Str(s) => format!("\"{}\"", escape_js_str(s)),
        ExprKind::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(|e| render_js(e, 0)).collect();
            format!("[{}]", parts.join(", "))
        }
        ExprKind::List(items) => {
            let parts: Vec<String> = items.iter().map(|e| render_js(e, 0)).collect();
            format!("[{}]", parts.join(", "))
        }
        ExprKind::Var { name } => name.clone(),
        ExprKind::Call { callee, args, .. } => {
            let rendered_args: Vec<String> = args.iter().map(|a| render_js(a, 0)).collect();
            format!("{callee}({})", rendered_args.join(", "))
        }
        ExprKind::Lambda { params, body } => {
            let params_list: Vec<String> = params.iter().map(|(p, _)| p.clone()).collect();
            let body_js = render_js(body, 0);
            format!("(function({}) {{ return {body_js}; }})", params_list.join(", "))
        }
        ExprKind::Block(stmts) => render_block(stmts),
        ExprKind::Match { expr, arms } => render_match(expr, arms),
        ExprKind::Binary { op, left, right } => {
            let prec = binop_precedence(op);
            let op_str = binop_symbol(op);
            let l = render_js(left, prec);
            let r = render_js(right, prec);
            let expr_str = format!("{l} {op_str} {r}");
            if prec < parent_prec {
                format!("({expr_str})")
            } else {
                expr_str
            }
        }
    }
}

pub fn render_annotated_program(stmts: &[Stmt], env: &crate::typecheck::TypeEnv) -> String {
    let mut lines = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, ty, expr, .. } => {
                let t = ty
                    .as_ref()
                    .or_else(|| env.schemes.get(name).map(|s| &s.ty))
                    .unwrap_or(&Type::Var(crate::ast::TypeVarId(0)));
                let js = render_js(expr, 0);
                lines.push(format!("let {name}: {} = {js};", type_name(t)));
            }
            Stmt::Fn { name, params, ret, body } => {
                let ret_ty = ret
                    .as_ref()
                    .or_else(|| env.schemes.get(name).map(|s| &s.ty))
                    .unwrap_or(&Type::Var(crate::ast::TypeVarId(0)));
                let params_str = params
                    .iter()
                    .map(|(p, t)| {
                        if let Some(t) = t {
                            format!("{p}: {}", type_name(t))
                        } else {
                            format!("{p}: {}", type_name(&Type::Var(crate::ast::TypeVarId(0))))
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let body_js = render_js(body, 0);
                lines.push(format!(
                    "fn {name}({params_str}): {} = {body_js};",
                    type_name(ret_ty)
                ));
            }
            Stmt::External { name, ty, js_name } => {
                lines.push(format!("external {name}: {} = \"{js_name}\";", type_name(ty)));
            }
            Stmt::Import { path } => {
                lines.push(format!("import \"{path}\";"));
            }
            Stmt::Expr(expr) => {
                let js = render_js(expr, 0);
                lines.push(js);
            }
        }
    }
    lines.join("\n")
}

fn render_number_literal(n: f64) -> String {
    if n.fract() == 0.0 {
        format!("{}", n as i64)
    } else {
        format!("{}", n)
    }
}

fn render_pattern_condition(var: &str, pat: &crate::ast::Pattern) -> String {
    match pat {
        crate::ast::Pattern::Wildcard => "true".to_string(),
        crate::ast::Pattern::Bind(_) => "true".to_string(),
        crate::ast::Pattern::Bool(b) => format!("{var} === {b}"),
        crate::ast::Pattern::Number(n) => format!("{var} === {}", render_number_literal(*n)),
        crate::ast::Pattern::Str(s) => format!("{var} === \"{}\"", escape_js_str(s)),
        crate::ast::Pattern::Tuple(items) => {
            let mut conds = vec![format!("Array.isArray({var})"), format!("{var}.length === {}", items.len())];
            for (i, pat) in items.iter().enumerate() {
                let inner_var = format!("{var}[{i}]");
                conds.push(render_pattern_condition(&inner_var, pat));
            }
            conds.join(" && ")
        }
        crate::ast::Pattern::List { items, rest } => {
            let len_cond = if rest.is_some() {
                format!("{var}.length >= {}", items.len())
            } else {
                format!("{var}.length === {}", items.len())
            };
            let mut conds = vec![format!("Array.isArray({var})"), len_cond];
            for (i, pat) in items.iter().enumerate() {
                let inner_var = format!("{var}[{i}]");
                conds.push(render_pattern_condition(&inner_var, pat));
            }
            conds.join(" && ")
        }
    }
}

fn pattern_bindings(var: &str, pat: &crate::ast::Pattern) -> Vec<(String, String)> {
    match pat {
        crate::ast::Pattern::Bind(name) => vec![(name.clone(), var.to_string())],
        crate::ast::Pattern::Tuple(items) => {
            let mut out = Vec::new();
            for (i, p) in items.iter().enumerate() {
                out.extend(pattern_bindings(&format!("{var}[{i}]"), p));
            }
            out
        }
        crate::ast::Pattern::List { items, rest } => {
            let mut out = Vec::new();
            for (i, p) in items.iter().enumerate() {
                out.extend(pattern_bindings(&format!("{var}[{i}]"), p));
            }
            if let Some(r) = rest {
                if !r.is_empty() {
                    out.push((r.clone(), format!("{var}.slice({})", items.len())));
                }
            }
            out
        }
        _ => Vec::new(),
    }
}

fn needs_fix_prelude(stmts: &[Stmt]) -> bool {
    let mut defined = false;
    let mut used = false;
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, expr, .. } => {
                if name == "fix" {
                    defined = true;
                }
                if expr_uses_fix(expr) {
                    used = true;
                }
            }
            Stmt::Fn { name, body, .. } => {
                if name == "fix" {
                    defined = true;
                }
                if expr_uses_fix(body) {
                    used = true;
                }
            }
            Stmt::External { name, .. } => {
                if name == "fix" {
                    defined = true;
                }
            }
            Stmt::Import { .. } => {
                // Import statements don't define or use fix
            }
            Stmt::Expr(expr) => {
                if expr_uses_fix(expr) {
                    used = true;
                }
            }
        }
    }
    used && !defined
}

fn expr_uses_fix(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Var { name } => name == "fix",
        ExprKind::Call { callee, args, .. } => callee == "fix" || args.iter().any(expr_uses_fix),
        ExprKind::Block(stmts) => stmts.iter().any(|s| match s {
            Stmt::Expr(e) => expr_uses_fix(e),
            Stmt::Let { expr, .. } => expr_uses_fix(expr),
            Stmt::Fn { body, .. } => expr_uses_fix(body),
            Stmt::External { .. } | Stmt::Import { .. } => false,
        }),
        ExprKind::Match { expr, arms } => expr_uses_fix(expr) || arms.iter().any(|a| expr_uses_fix(&a.expr)),
        ExprKind::Binary { left, right, .. } => expr_uses_fix(left) || expr_uses_fix(right),
        ExprKind::Tuple(items) | ExprKind::List(items) => items.iter().any(expr_uses_fix),
        ExprKind::Lambda { body, .. } => expr_uses_fix(body),
        ExprKind::Number(_) | ExprKind::Bool(_) | ExprKind::Str(_) => false,
    }
}

fn render_block(stmts: &[Stmt]) -> String {
    let mut parts = Vec::new();
    let last_expr = match stmts.last() {
        Some(Stmt::Expr(e)) => e,
        _ => unreachable!("parser enforces block ending with expression"),
    };
    for stmt in stmts.iter().take(stmts.len().saturating_sub(1)) {
        match stmt {
            Stmt::Let { name, expr, .. } => {
                let js = render_js(expr, 0);
                parts.push(format!("let {name} = {js};"));
            }
            Stmt::Expr(expr) => {
                let js = render_js(expr, 0);
                parts.push(format!("{js};"));
            }
            Stmt::Fn { name, params, body, .. } => {
                let js_body = render_js(body, 0);
                let param_list: Vec<String> = params.iter().map(|(p, _)| p.clone()).collect();
                parts.push(format!("function {name}({}) {{ return {js_body}; }}", param_list.join(", ")));
            }
            Stmt::External { name, js_name, .. } => {
                parts.push(format!("const {name} = {js_name};"));
            }
            Stmt::Import { .. } => {
                // Import statements are resolved before rendering
            }
        }
    }
    let last = render_js(last_expr, 0);
    parts.push(format!("return {last};"));
    format!("(() => {{ {} }})()", parts.join(" "))
}

fn render_match(expr: &Expr, arms: &[MatchArm]) -> String {
    let scrutinee = render_js(expr, 0);
    let mut parts = Vec::new();
    parts.push(format!("const __match = {scrutinee};"));
    for (i, arm) in arms.iter().enumerate() {
        let cond = render_pattern_condition("__match", &arm.pat);
        let bindings = pattern_bindings("__match", &arm.pat);
        let decls: Vec<String> = bindings.iter().map(|(n, e)| format!("const {n} = {e};")).collect();
        let binds_js = if decls.is_empty() { String::new() } else { format!("{} ", decls.join(" ")) };
        let body = render_js(&arm.expr, 0);
        if i == 0 {
            parts.push(format!("if ({cond}) {{ {binds_js}return {body}; }}"));
        } else {
            parts.push(format!("else if ({cond}) {{ {binds_js}return {body}; }}"));
        }
    }
    parts.push("return undefined;".to_string());
    format!("(() => {{ {} }})()", parts.join(" "))
}

fn escape_js_str(s: &str) -> String {
    s.chars()
        .flat_map(|c| match c {
            '\\' => "\\\\".chars().collect::<Vec<_>>(),
            '"' => "\\\"".chars().collect::<Vec<_>>(),
            '\n' => "\\n".chars().collect::<Vec<_>>(),
            '\r' => "\\r".chars().collect::<Vec<_>>(),
            '\t' => "\\t".chars().collect::<Vec<_>>(),
            other => vec![other],
        })
        .collect()
}
