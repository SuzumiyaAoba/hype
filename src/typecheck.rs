use std::collections::HashMap;
use std::ops::Range;

use crate::ast::{Expr, ExprKind, FnSig, MatchArm, Pattern, Stmt, Type};
use crate::debug::DebugInfo;
use crate::error::ParseError;

#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    pub vars: HashMap<String, Type>,
    pub fns: HashMap<String, FnSig>,
}

pub fn type_name(t: &Type) -> &'static str {
    match t {
        Type::Number => "Number",
        Type::String => "String",
        Type::Bool => "Bool",
        Type::Unknown => "Unknown",
    }
}

pub(crate) fn typecheck(
    stmts: &[Stmt],
    source: &str,
    mut debug: Option<&mut DebugInfo>,
) -> Result<TypeEnv, ParseError> {
    let mut fns: HashMap<String, FnSig> = HashMap::new();
    for stmt in stmts {
        if let Stmt::Fn { name, params, ret, .. } = stmt {
            if fns.contains_key(name) {
                return Err(ParseError {
                    message: format!("duplicate function '{}'", name),
                    span: 0..0,
                    source: source.to_string(),
                });
            }
            let param_tys = params.iter().map(|(_, t)| t.clone()).collect();
            fns.insert(
                name.clone(),
                FnSig {
                    params: param_tys,
                    ret: ret.clone().unwrap_or(Type::Unknown),
                },
            );
            crate::debug::log_debug(
                debug.as_deref_mut(),
                || {
                    format!(
                        "fn signature collected: {name}({}) -> {:?}",
                        params
                            .iter()
                            .map(|(p, t)| format!("{p}: {:?}", t))
                            .collect::<Vec<_>>()
                            .join(", "),
                        ret.as_ref().unwrap_or(&Type::Unknown)
                    )
                },
            );
        }
    }

    let mut vars: HashMap<String, Type> = HashMap::new();
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, ty, expr } => {
                let expr_ty = type_of_expr(expr, &vars, &fns, source, debug.as_deref_mut())?;
                if let Some(t) = ty {
                    unify_or_error(t, &expr_ty, source, "type mismatch for let binding", &expr.span)?;
                    vars.insert(name.clone(), t.clone());
                    crate::debug::log_debug(
                        debug.as_deref_mut(),
                        || format!("let {name}: {:?} = (checked as {:?})", t, expr_ty),
                    );
                } else {
                    crate::debug::log_debug(
                        debug.as_deref_mut(),
                        || format!("let {name} inferred as {:?}", expr_ty),
                    );
                    vars.insert(name.clone(), expr_ty);
                }
            }
            Stmt::Fn { name, params, ret, body } => {
                let mut local = vars.clone();
                for (pname, pty) in params {
                    local.insert(pname.clone(), pty.clone());
                }
                let body_ty = type_of_expr(body, &local, &fns, source, debug.as_deref_mut())?;
                let sig = fns.get_mut(name).expect("fn sig exists");
                if let Some(r) = ret {
                    unify_or_error(r, &body_ty, source, "type mismatch in function body", &body.span)?;
                    sig.ret = r.clone();
                    crate::debug::log_debug(
                        debug.as_deref_mut(),
                        || format!("fn {name} return checked as {:?} (body {:?})", r, body_ty),
                    );
                } else {
                    sig.ret = body_ty;
                    crate::debug::log_debug(
                        debug.as_deref_mut(),
                        || format!("fn {name} return inferred as {:?}", sig.ret),
                    );
                }
            }
            Stmt::Expr(expr) => {
                let _ = type_of_expr(expr, &vars, &fns, source, debug.as_deref_mut())?;
            }
        }
    }
    Ok(TypeEnv { vars, fns })
}

fn unify_or_error(
    expected: &Type,
    found: &Type,
    source: &str,
    msg: &str,
    span: &Range<usize>,
) -> Result<(), ParseError> {
    if let Some(_) = unify(expected, found) {
        Ok(())
    } else {
        Err(ParseError {
            message: format!("{msg}: expected {:?}, found {:?}", expected, found),
            span: span.clone(),
            source: source.to_string(),
        })
    }
}

fn unify(a: &Type, b: &Type) -> Option<Type> {
    match (a, b) {
        (Type::Unknown, t) | (t, Type::Unknown) => Some(t.clone()),
        (Type::Number, Type::Number) => Some(Type::Number),
        (Type::String, Type::String) => Some(Type::String),
        (Type::Bool, Type::Bool) => Some(Type::Bool),
        _ => None,
    }
}

fn ensure_pattern_matches(
    pat: &Pattern,
    ty: &Type,
    source: &str,
    span: &Range<usize>,
) -> Result<(), ParseError> {
    let ok = match pat {
        Pattern::Wildcard => true,
        _ if matches!(ty, Type::Unknown) => true,
        Pattern::Bool(_) => matches!(ty, Type::Bool),
        Pattern::Number(_) => matches!(ty, Type::Number),
        Pattern::Str(_) => matches!(ty, Type::String),
    };
    if ok {
        Ok(())
    } else {
        Err(ParseError {
            message: format!("pattern does not match type {:?}", ty),
            span: span.clone(),
            source: source.to_string(),
        })
    }
}

fn type_of_match(
    expr: &Expr,
    arms: &[MatchArm],
    vars: &HashMap<String, Type>,
    fns: &HashMap<String, FnSig>,
    source: &str,
    mut debug: Option<&mut DebugInfo>,
    span: &Range<usize>,
) -> Result<Type, ParseError> {
    if arms.is_empty() {
        return Err(ParseError {
            message: "match requires at least one arm".into(),
            span: span.clone(),
            source: source.to_string(),
        });
    }

    let scrutinee_ty = type_of_expr(expr, vars, fns, source, debug.as_deref_mut())?;
    let mut result_ty: Option<Type> = None;
    for arm in arms {
        ensure_pattern_matches(&arm.pat, &scrutinee_ty, source, span)?;
        let ty = type_of_expr(&arm.expr, vars, fns, source, debug.as_deref_mut())?;
        if let Some(prev) = &result_ty {
            if let Some(u) = unify(prev, &ty) {
                result_ty = Some(u);
            } else {
                return Err(ParseError {
                    message: format!(
                        "match arms must have the same type, found {:?} and {:?}",
                        prev, ty
                    ),
                    span: arm.expr.span.clone(),
                    source: source.to_string(),
                });
            }
        } else {
            result_ty = Some(ty);
        }
    }

    result_ty.ok_or_else(|| ParseError {
        message: "match requires at least one arm".into(),
        span: span.clone(),
        source: source.to_string(),
    })
}

fn type_of_block(
    stmts: &[Stmt],
    vars: &HashMap<String, Type>,
    fns: &HashMap<String, FnSig>,
    source: &str,
    mut debug: Option<&mut DebugInfo>,
    span: &Range<usize>,
) -> Result<Type, ParseError> {
    if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
        return Err(ParseError {
            message: "block must end with expression".into(),
            span: span.clone(),
            source: source.to_string(),
        });
    }
    let mut local = vars.clone();
    let mut last_ty = None;
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, ty, expr } => {
                let expr_ty = type_of_expr(expr, &local, fns, source, debug.as_deref_mut())?;
                if let Some(t) = ty {
                    unify_or_error(t, &expr_ty, source, "type mismatch in block let", &expr.span)?;
                    local.insert(name.clone(), t.clone());
                } else {
                    local.insert(name.clone(), expr_ty);
                }
            }
            Stmt::Expr(expr) => {
                let ty = type_of_expr(expr, &local, fns, source, debug.as_deref_mut())?;
                last_ty = Some(ty);
            }
            Stmt::Fn { .. } => {
                return Err(ParseError {
                    message: "functions inside blocks are not supported".into(),
                    span: span.clone(),
                    source: source.to_string(),
                });
            }
        }
    }
    last_ty.ok_or_else(|| ParseError {
        message: "block must end with expression".into(),
        span: span.clone(),
        source: source.to_string(),
    })
}

fn type_of_expr(
    expr: &Expr,
    vars: &HashMap<String, Type>,
    fns: &HashMap<String, FnSig>,
    source: &str,
    mut debug: Option<&mut DebugInfo>,
) -> Result<Type, ParseError> {
    match &expr.kind {
        ExprKind::Number(_) => Ok(Type::Number),
        ExprKind::Bool(_) => Ok(Type::Bool),
        ExprKind::Str(_) => Ok(Type::String),
        ExprKind::Block(stmts) => type_of_block(stmts, vars, fns, source, debug, &expr.span),
        ExprKind::Match { expr, arms } => type_of_match(expr, arms, vars, fns, source, debug, &expr.span),
        ExprKind::Var { name } => vars.get(name).cloned().ok_or_else(|| ParseError {
            message: format!("unbound variable '{}'", name),
            span: expr.span.clone(),
            source: source.to_string(),
        }),
        ExprKind::Binary { op, left, right } => {
            let lt = type_of_expr(left, vars, fns, source, debug.as_deref_mut())?;
            let rt = type_of_expr(right, vars, fns, source, debug.as_deref_mut())?;
            match op {
                crate::ast::BinOp::Add => {
                    if let Some(t) = unify(&lt, &rt) {
                        match t {
                            Type::Number => Ok(Type::Number),
                            Type::String => Ok(Type::String),
                            _ => Err(ParseError {
                                message: "type mismatch for '+'".into(),
                                span: expr.span.clone(),
                                source: source.to_string(),
                            }),
                        }
                    } else {
                        Err(ParseError {
                            message: "type mismatch for '+'".into(),
                            span: expr.span.clone(),
                            source: source.to_string(),
                        })
                    }
                }
                crate::ast::BinOp::Sub | crate::ast::BinOp::Mul | crate::ast::BinOp::Div => {
                    unify_or_error(&Type::Number, &lt, source, "numeric operator requires Number", &expr.span)?;
                    unify_or_error(&Type::Number, &rt, source, "numeric operator requires Number", &expr.span)?;
                    Ok(Type::Number)
                }
                crate::ast::BinOp::Eq | crate::ast::BinOp::Ne => {
                    if unify(&lt, &rt).is_some() {
                        Ok(Type::Bool)
                    } else {
                        Err(ParseError {
                            message: "equality requires operands of the same type".into(),
                            span: expr.span.clone(),
                            source: source.to_string(),
                        })
                    }
                }
                crate::ast::BinOp::Lt | crate::ast::BinOp::Gt | crate::ast::BinOp::Le | crate::ast::BinOp::Ge => {
                    unify_or_error(&Type::Number, &lt, source, "comparison requires Number operands", &expr.span)?;
                    unify_or_error(&Type::Number, &rt, source, "comparison requires Number operands", &expr.span)?;
                    Ok(Type::Bool)
                }
                crate::ast::BinOp::And | crate::ast::BinOp::Or => {
                    unify_or_error(&Type::Bool, &lt, source, "logical operator requires Bool", &expr.span)?;
                    unify_or_error(&Type::Bool, &rt, source, "logical operator requires Bool", &expr.span)?;
                    Ok(Type::Bool)
                }
            }
        }
        ExprKind::Call { callee, callee_span, args } => {
            let sig = fns.get(callee).ok_or_else(|| ParseError {
                message: format!("unknown function '{}'", callee),
                span: callee_span.clone(),
                source: source.to_string(),
            })?;
            if sig.params.len() != args.len() {
                return Err(ParseError {
                    message: format!(
                        "arity mismatch: expected {}, found {}",
                        sig.params.len(),
                        args.len()
                    ),
                    span: expr.span.clone(),
                    source: source.to_string(),
                });
            }
            for (arg, expected) in args.iter().zip(sig.params.iter()) {
                let aty = type_of_expr(arg, vars, fns, source, debug.as_deref_mut())?;
                if &aty != expected {
                    return Err(ParseError {
                        message: format!(
                            "type mismatch in argument: expected {:?}, found {:?}",
                            expected, aty
                        ),
                        span: arg.span.clone(),
                        source: source.to_string(),
                    });
                }
            }
            Ok(sig.ret.clone())
        }
    }
}
