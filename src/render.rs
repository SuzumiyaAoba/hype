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

pub fn render_js(expr: &Expr, parent_prec: u8) -> String {
    match &expr.kind {
        ExprKind::Number(n) => {
            render_number_literal(*n)
        }
        ExprKind::Bool(b) => format!("{b}"),
        ExprKind::Str(s) => format!("\"{}\"", escape_js_str(s)),
        ExprKind::Var { name } => name.clone(),
        ExprKind::Call { callee, args, .. } => {
            let rendered_args: Vec<String> = args.iter().map(|a| render_js(a, 0)).collect();
            format!("{callee}({})", rendered_args.join(", "))
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
            Stmt::Let { name, ty, expr } => {
                let t = ty
                    .as_ref()
                    .or_else(|| env.vars.get(name))
                    .unwrap_or(&Type::Unknown);
                let js = render_js(expr, 0);
                lines.push(format!("let {name}: {} = {js};", type_name(t)));
            }
            Stmt::Fn { name, params, ret, body } => {
                let ret_ty = ret
                    .as_ref()
                    .or_else(|| env.fns.get(name).map(|f| &f.ret))
                    .unwrap_or(&Type::Unknown);
                let params_str = params
                    .iter()
                    .map(|(p, t)| format!("{p}: {}", type_name(t)))
                    .collect::<Vec<_>>()
                    .join(", ");
                let body_js = render_js(body, 0);
                lines.push(format!(
                    "fn {name}({params_str}): {} = {body_js};",
                    type_name(ret_ty)
                ));
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
        crate::ast::Pattern::Bool(b) => format!("{var} === {b}"),
        crate::ast::Pattern::Number(n) => format!("{var} === {}", render_number_literal(*n)),
        crate::ast::Pattern::Str(s) => format!("{var} === \"{}\"", escape_js_str(s)),
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
            Stmt::Fn { .. } => {}
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
        let body = render_js(&arm.expr, 0);
        if i == 0 {
            parts.push(format!("if ({cond}) {{ return {body}; }}"));
        } else {
            parts.push(format!("else if ({cond}) {{ return {body}; }}"));
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
