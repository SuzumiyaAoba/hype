use crate::ast::{BinOp, Expr, ExprKind, MatchArm, Stmt, Type};
use crate::typecheck::type_name;

/// Context for TCO transformation
struct TcoContext {
    /// The name of the function being optimized
    fn_name: String,
    /// Parameter names for reassignment
    params: Vec<String>,
}

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
                let param_list: Vec<String> = params.iter().map(|(p, _)| p.clone()).collect();

                // Check if this function is tail-recursive
                if is_tail_recursive(name, body) {
                    // Render with TCO optimization
                    let tco_ctx = TcoContext {
                        fn_name: name.clone(),
                        params: param_list.clone(),
                    };
                    let tco_body = render_tco_body(body, &tco_ctx);
                    lines.push(format!(
                        "function {name}({}) {{ while (true) {{ {} }} }}",
                        param_list.join(", "),
                        tco_body
                    ));
                } else {
                    let js_body = render_js(body, 0);
                    lines.push(format!(
                        "function {name}({}) {{ return {js_body}; }}",
                        param_list.join(", ")
                    ));
                }
            }
            Stmt::External { name, js_name, .. } => {
                lines.push(format!("const {name} = {js_name};"));
            }
            Stmt::Import { .. } => {
                // Import statements are resolved before rendering
            }
            Stmt::TypeDecl { .. } => {
                // Type declarations don't produce JS output
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
        ExprKind::Call { callee, args } => {
            let callee_js = render_js(callee, 0);
            let rendered_args: Vec<String> = args.iter().map(|a| render_js(a, 0)).collect();
            format!("{callee_js}({})", rendered_args.join(", "))
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
        ExprKind::Constructor { name, fields } => {
            if fields.is_empty() {
                format!("{{ __tag: \"{}\" }}", name)
            } else {
                let field_parts: Vec<String> = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, render_js(v, 0)))
                    .collect();
                format!("{{ __tag: \"{}\", {} }}", name, field_parts.join(", "))
            }
        }
        ExprKind::Record(fields) => {
            let field_parts: Vec<String> = fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, render_js(v, 0)))
                .collect();
            format!("{{ {} }}", field_parts.join(", "))
        }
        ExprKind::FieldAccess { expr, field } => {
            let expr_js = render_js(expr, 0);
            format!("{}.{}", expr_js, field)
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
            Stmt::TypeDecl { name, params, variants } => {
                let params_str = if params.is_empty() {
                    String::new()
                } else {
                    format!("<{}>", params.join(", "))
                };
                let variants_str: Vec<String> = variants
                    .iter()
                    .map(|v| {
                        if v.fields.is_empty() {
                            v.name.clone()
                        } else {
                            let fields: Vec<String> = v
                                .fields
                                .iter()
                                .map(|(n, t)| format!("{}: {}", n, type_name(t)))
                                .collect();
                            format!("{} {{ {} }}", v.name, fields.join(", "))
                        }
                    })
                    .collect();
                lines.push(format!("type {}{} = {};", name, params_str, variants_str.join(" | ")));
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
        crate::ast::Pattern::Constructor { name, fields } => {
            let mut conds = vec![format!("{var}.__tag === \"{}\"", name)];
            for (field_name, field_pat) in fields {
                let inner_var = format!("{var}.{field_name}");
                let cond = render_pattern_condition(&inner_var, field_pat);
                if cond != "true" {
                    conds.push(cond);
                }
            }
            conds.join(" && ")
        }
        crate::ast::Pattern::Record { fields } => {
            let mut conds = vec![format!("typeof {var} === \"object\"")];
            for (field_name, field_pat) in fields {
                let inner_var = format!("{var}.{field_name}");
                let cond = render_pattern_condition(&inner_var, field_pat);
                if cond != "true" {
                    conds.push(cond);
                }
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
        crate::ast::Pattern::Constructor { fields, .. } => {
            let mut out = Vec::new();
            for (field_name, field_pat) in fields {
                out.extend(pattern_bindings(&format!("{var}.{field_name}"), field_pat));
            }
            out
        }
        crate::ast::Pattern::Record { fields } => {
            let mut out = Vec::new();
            for (field_name, field_pat) in fields {
                out.extend(pattern_bindings(&format!("{var}.{field_name}"), field_pat));
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
            Stmt::TypeDecl { .. } => {
                // Type declarations don't define or use fix
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
        ExprKind::Call { callee, args } => expr_uses_fix(callee) || args.iter().any(expr_uses_fix),
        ExprKind::Block(stmts) => stmts.iter().any(|s| match s {
            Stmt::Expr(e) => expr_uses_fix(e),
            Stmt::Let { expr, .. } => expr_uses_fix(expr),
            Stmt::Fn { body, .. } => expr_uses_fix(body),
            Stmt::External { .. } | Stmt::Import { .. } | Stmt::TypeDecl { .. } => false,
        }),
        ExprKind::Match { expr, arms } => expr_uses_fix(expr) || arms.iter().any(|a| expr_uses_fix(&a.expr)),
        ExprKind::Binary { left, right, .. } => expr_uses_fix(left) || expr_uses_fix(right),
        ExprKind::Tuple(items) | ExprKind::List(items) => items.iter().any(expr_uses_fix),
        ExprKind::Lambda { body, .. } => expr_uses_fix(body),
        ExprKind::Constructor { fields, .. } => fields.iter().any(|(_, e)| expr_uses_fix(e)),
        ExprKind::Record(fields) => fields.iter().any(|(_, e)| expr_uses_fix(e)),
        ExprKind::FieldAccess { expr, .. } => expr_uses_fix(expr),
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
            Stmt::TypeDecl { .. } => {
                // Type declarations don't produce JS output
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
        let pat_cond = render_pattern_condition("__match", &arm.pat);
        let bindings = pattern_bindings("__match", &arm.pat);
        let decls: Vec<String> = bindings.iter().map(|(n, e)| format!("const {n} = {e};")).collect();
        let binds_js = if decls.is_empty() { String::new() } else { format!("{} ", decls.join(" ")) };
        let body = render_js(&arm.expr, 0);

        // Pattern condition (guard is checked after bindings are established)
        let cond = pat_cond;

        // If there's a guard, add it as an inner check after bindings
        let body_with_guard = if let Some(guard) = &arm.guard {
            let guard_js = render_js(guard, 0);
            format!("if ({guard_js}) {{ return {body}; }}")
        } else {
            format!("return {body};")
        };

        if i == 0 {
            parts.push(format!("if ({cond}) {{ {binds_js}{body_with_guard} }}"));
        } else {
            parts.push(format!("else if ({cond}) {{ {binds_js}{body_with_guard} }}"));
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

// ============================================================================
// Tail Call Optimization (TCO)
// ============================================================================

/// Check if a function is tail-recursive (has self-recursive calls only in tail position)
fn is_tail_recursive(fn_name: &str, body: &Expr) -> bool {
    // First check if there are any self-recursive calls at all
    if !has_self_call(fn_name, body) {
        return false;
    }

    // Check that ALL self-calls are in tail position
    all_self_calls_in_tail_position(fn_name, body, true)
}

/// Check if the expression contains any call to the given function
fn has_self_call(fn_name: &str, expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { callee, args } => {
            if let ExprKind::Var { name } = &callee.kind {
                if name == fn_name {
                    return true;
                }
            }
            has_self_call(fn_name, callee) || args.iter().any(|a| has_self_call(fn_name, a))
        }
        ExprKind::Match { expr, arms } => {
            has_self_call(fn_name, expr)
                || arms.iter().any(|arm| {
                    has_self_call(fn_name, &arm.expr)
                        || arm.guard.as_ref().map_or(false, |g| has_self_call(fn_name, g))
                })
        }
        ExprKind::Block(stmts) => stmts.iter().any(|s| match s {
            Stmt::Expr(e) => has_self_call(fn_name, e),
            Stmt::Let { expr, .. } => has_self_call(fn_name, expr),
            Stmt::Fn { body, .. } => has_self_call(fn_name, body),
            _ => false,
        }),
        ExprKind::Binary { left, right, .. } => {
            has_self_call(fn_name, left) || has_self_call(fn_name, right)
        }
        ExprKind::Lambda { body, .. } => has_self_call(fn_name, body),
        ExprKind::Tuple(items) | ExprKind::List(items) => {
            items.iter().any(|e| has_self_call(fn_name, e))
        }
        ExprKind::Constructor { fields, .. } | ExprKind::Record(fields) => {
            fields.iter().any(|(_, e)| has_self_call(fn_name, e))
        }
        ExprKind::FieldAccess { expr, .. } => has_self_call(fn_name, expr),
        ExprKind::Number(_) | ExprKind::Bool(_) | ExprKind::Str(_) | ExprKind::Var { .. } => false,
    }
}

/// Check that all self-calls are in tail position
/// `in_tail` indicates whether the current expression is in tail position
fn all_self_calls_in_tail_position(fn_name: &str, expr: &Expr, in_tail: bool) -> bool {
    match &expr.kind {
        ExprKind::Call { callee, args } => {
            // Check if this is a self-call
            if let ExprKind::Var { name } = &callee.kind {
                if name == fn_name {
                    // Self-call must be in tail position
                    if !in_tail {
                        return false;
                    }
                    // Arguments must not contain self-calls
                    return args.iter().all(|a| !has_self_call(fn_name, a));
                }
            }
            // Non-self calls: check callee and args (not in tail position)
            all_self_calls_in_tail_position(fn_name, callee, false)
                && args
                    .iter()
                    .all(|a| all_self_calls_in_tail_position(fn_name, a, false))
        }
        ExprKind::Match { expr: scrutinee, arms } => {
            // Scrutinee is not in tail position
            if !all_self_calls_in_tail_position(fn_name, scrutinee, false) {
                return false;
            }
            // Each arm body inherits the tail position
            arms.iter().all(|arm| {
                // Guard is not in tail position
                if let Some(guard) = &arm.guard {
                    if !all_self_calls_in_tail_position(fn_name, guard, false) {
                        return false;
                    }
                }
                all_self_calls_in_tail_position(fn_name, &arm.expr, in_tail)
            })
        }
        ExprKind::Block(stmts) => {
            // Only the last expression is in tail position
            for (i, stmt) in stmts.iter().enumerate() {
                let is_last = i == stmts.len() - 1;
                match stmt {
                    Stmt::Expr(e) => {
                        if !all_self_calls_in_tail_position(fn_name, e, is_last && in_tail) {
                            return false;
                        }
                    }
                    Stmt::Let { expr, .. } => {
                        if !all_self_calls_in_tail_position(fn_name, expr, false) {
                            return false;
                        }
                    }
                    Stmt::Fn { body, .. } => {
                        // Nested function definitions: self-calls here refer to the outer fn
                        // They're not in tail position of the outer function
                        if has_self_call(fn_name, body) {
                            return false;
                        }
                    }
                    _ => {}
                }
            }
            true
        }
        ExprKind::Binary { left, right, .. } => {
            // Operands are not in tail position
            all_self_calls_in_tail_position(fn_name, left, false)
                && all_self_calls_in_tail_position(fn_name, right, false)
        }
        ExprKind::Lambda { body, .. } => {
            // Lambda body: self-calls refer to outer function, not in tail position
            !has_self_call(fn_name, body)
        }
        ExprKind::Tuple(items) | ExprKind::List(items) => items
            .iter()
            .all(|e| all_self_calls_in_tail_position(fn_name, e, false)),
        ExprKind::Constructor { fields, .. } | ExprKind::Record(fields) => fields
            .iter()
            .all(|(_, e)| all_self_calls_in_tail_position(fn_name, e, false)),
        ExprKind::FieldAccess { expr, .. } => {
            all_self_calls_in_tail_position(fn_name, expr, false)
        }
        ExprKind::Number(_) | ExprKind::Bool(_) | ExprKind::Str(_) | ExprKind::Var { .. } => true,
    }
}

/// Render the body of a tail-recursive function with TCO
fn render_tco_body(expr: &Expr, ctx: &TcoContext) -> String {
    render_tco_expr(expr, ctx, true)
}

/// Render an expression with TCO transformation
/// `in_tail` indicates whether the current expression is in tail position
fn render_tco_expr(expr: &Expr, ctx: &TcoContext, in_tail: bool) -> String {
    match &expr.kind {
        ExprKind::Call { callee, args } => {
            // Check if this is a tail-recursive self-call
            if in_tail {
                if let ExprKind::Var { name } = &callee.kind {
                    if name == &ctx.fn_name {
                        // Transform tail call to parameter reassignment + continue
                        let rendered_args: Vec<String> =
                            args.iter().map(|a| render_js(a, 0)).collect();

                        if ctx.params.len() == 1 {
                            // Single parameter: simple assignment
                            return format!(
                                "{} = {}; continue;",
                                ctx.params[0], rendered_args[0]
                            );
                        } else {
                            // Multiple parameters: use destructuring to avoid order issues
                            return format!(
                                "[{}] = [{}]; continue;",
                                ctx.params.join(", "),
                                rendered_args.join(", ")
                            );
                        }
                    }
                }
            }
            // Non-tail call or not a self-call
            let js = render_js(expr, 0);
            if in_tail {
                format!("return {};", js)
            } else {
                js
            }
        }
        ExprKind::Match { expr: scrutinee, arms } => {
            render_tco_match(scrutinee, arms, ctx, in_tail)
        }
        ExprKind::Block(stmts) => render_tco_block(stmts, ctx, in_tail),
        _ => {
            // For other expressions, just render normally
            let js = render_js(expr, 0);
            if in_tail {
                format!("return {};", js)
            } else {
                js
            }
        }
    }
}

/// Render a match expression with TCO
fn render_tco_match(scrutinee: &Expr, arms: &[MatchArm], ctx: &TcoContext, in_tail: bool) -> String {
    let scrutinee_js = render_js(scrutinee, 0);
    let mut parts = Vec::new();
    parts.push(format!("const __match = {};", scrutinee_js));

    for (i, arm) in arms.iter().enumerate() {
        let pat_cond = render_pattern_condition("__match", &arm.pat);
        let bindings = pattern_bindings("__match", &arm.pat);
        let decls: Vec<String> = bindings
            .iter()
            .map(|(n, e)| format!("const {n} = {e};"))
            .collect();
        let binds_js = if decls.is_empty() {
            String::new()
        } else {
            format!("{} ", decls.join(" "))
        };

        // Render body with TCO
        let body_js = render_tco_expr(&arm.expr, ctx, in_tail);

        // Handle guard
        let body_with_guard = if let Some(guard) = &arm.guard {
            let guard_js = render_js(guard, 0);
            format!("if ({guard_js}) {{ {body_js} }}")
        } else {
            body_js
        };

        if i == 0 {
            parts.push(format!("if ({pat_cond}) {{ {binds_js}{body_with_guard} }}"));
        } else {
            parts.push(format!(
                "else if ({pat_cond}) {{ {binds_js}{body_with_guard} }}"
            ));
        }
    }

    if in_tail {
        parts.push("return undefined;".to_string());
    }

    parts.join(" ")
}

/// Render a block with TCO
fn render_tco_block(stmts: &[Stmt], ctx: &TcoContext, in_tail: bool) -> String {
    let mut parts = Vec::new();

    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;

        match stmt {
            Stmt::Let { name, expr, .. } => {
                let js = render_js(expr, 0);
                parts.push(format!("let {name} = {js};"));
            }
            Stmt::Expr(e) => {
                if is_last && in_tail {
                    parts.push(render_tco_expr(e, ctx, true));
                } else {
                    let js = render_js(e, 0);
                    if !is_last {
                        parts.push(format!("{js};"));
                    } else {
                        parts.push(js);
                    }
                }
            }
            Stmt::Fn {
                name,
                params,
                body,
                ..
            } => {
                let js_body = render_js(body, 0);
                let param_list: Vec<String> = params.iter().map(|(p, _)| p.clone()).collect();
                parts.push(format!(
                    "function {name}({}) {{ return {js_body}; }}",
                    param_list.join(", ")
                ));
            }
            Stmt::External { name, js_name, .. } => {
                parts.push(format!("const {name} = {js_name};"));
            }
            Stmt::Import { .. } | Stmt::TypeDecl { .. } => {}
        }
    }

    parts.join(" ")
}
