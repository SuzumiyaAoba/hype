use std::collections::{HashMap, HashSet};
use std::ops::Range;

use crate::ast::{Expr, ExprKind, MatchArm, Pattern, Scheme, Stmt, Type, TypeVarId};
use crate::debug::DebugInfo;
use crate::error::ParseError;

#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    pub schemes: HashMap<String, Scheme>,
}

pub fn type_name(t: &Type) -> String {
    match t {
        Type::Number => "Number".into(),
        Type::String => "String".into(),
        Type::Bool => "Bool".into(),
        Type::Var(tv) => format!("t{}", tv.0),
        Type::Fun(params, ret) => {
            let ps: Vec<String> = params.iter().map(type_name).collect();
            format!("({}) -> {}", ps.join(", "), type_name(ret))
        }
    }
}

#[derive(Debug, Clone, Default)]
struct Subst(HashMap<TypeVarId, Type>);

impl Subst {
    fn new() -> Self {
        Subst(HashMap::new())
    }

    fn apply(&self, t: &Type) -> Type {
        match t {
            Type::Var(tv) => self.0.get(tv).cloned().unwrap_or(Type::Var(tv.clone())),
            Type::Fun(ps, r) => Type::Fun(ps.iter().map(|p| self.apply(p)).collect(), Box::new(self.apply(r))),
            other => other.clone(),
        }
    }

    fn compose(&self, other: &Subst) -> Subst {
        let mut map = other
            .0
            .iter()
            .map(|(k, v)| (k.clone(), self.apply(v)))
            .collect::<HashMap<_, _>>();
        for (k, v) in self.0.iter() {
            map.insert(k.clone(), v.clone());
        }
        Subst(map)
    }
}

fn free_type_vars(t: &Type, acc: &mut HashSet<TypeVarId>) {
    match t {
        Type::Var(tv) => {
            acc.insert(tv.clone());
        }
        Type::Fun(ps, r) => {
            for p in ps {
                free_type_vars(p, acc);
            }
            free_type_vars(r, acc);
        }
        _ => {}
    }
}

fn free_vars_scheme(s: &Scheme) -> HashSet<TypeVarId> {
    let mut set = HashSet::new();
    free_type_vars(&s.ty, &mut set);
    for v in &s.vars {
        set.remove(v);
    }
    set
}

fn free_vars_env(env: &TypeEnv) -> HashSet<TypeVarId> {
    let mut set = HashSet::new();
    for scheme in env.schemes.values() {
        set.extend(free_vars_scheme(scheme));
    }
    set
}

fn generalize(env: &TypeEnv, t: &Type) -> Scheme {
    let mut ftv = HashSet::new();
    free_type_vars(t, &mut ftv);
    let env_ftv = free_vars_env(env);
    ftv.retain(|v| !env_ftv.contains(v));
    Scheme {
        vars: ftv.into_iter().collect(),
        ty: t.clone(),
    }
}

fn instantiate(s: &Scheme, state: &mut InferState) -> Type {
    let mut subst = Subst::new();
    for v in &s.vars {
        subst.0.insert(v.clone(), state.fresh_var());
    }
    subst.apply(&s.ty)
}

#[derive(Default)]
struct InferState {
    counter: u32,
}

impl InferState {
    fn fresh_var(&mut self) -> Type {
        let id = self.counter;
        self.counter += 1;
        Type::Var(TypeVarId(id))
    }
}

pub(crate) fn typecheck(
    stmts: &[Stmt],
    source: &str,
    mut debug: Option<&mut DebugInfo>,
) -> Result<TypeEnv, ParseError> {
    let mut env = TypeEnv::default();
    let mut state = InferState::default();

    let mut fn_seeds: HashMap<String, (Vec<Type>, Type)> = HashMap::new();
    for stmt in stmts {
        if let Stmt::Fn { name, params, ret, .. } = stmt {
            let mut param_types = Vec::new();
            for (_, pty) in params {
                let pt = if let Some(t) = pty {
                    t.clone()
                } else {
                    state.fresh_var()
                };
                param_types.push(pt);
            }
            let ret_ty = ret.clone().unwrap_or_else(|| state.fresh_var());
            let fn_ty = Type::Fun(param_types.clone(), Box::new(ret_ty.clone()));
            env.schemes.insert(name.clone(), Scheme { vars: vec![], ty: fn_ty });
            fn_seeds.insert(name.clone(), (param_types, ret_ty));
        }
    }

    for stmt in stmts {
        match stmt {
            Stmt::Let { name, ty, expr } => {
                let (t, s) = infer_expr(expr, &env, &mut state, source)?;
                let t_app = s.apply(&t);
                if let Some(annot) = ty {
                    let ann = s.apply(annot);
                    unify(&t_app, &ann, &expr.span, source)?;
                    env.schemes.insert(name.clone(), generalize(&env, &ann));
                    crate::debug::log_debug(debug.as_deref_mut(), || {
                        format!("let {name} annotated as {:?}", ann)
                    });
                } else {
                    env.schemes.insert(name.clone(), generalize(&env, &t_app));
                    crate::debug::log_debug(debug.as_deref_mut(), || format!("let {name} inferred as {:?}", t_app));
                }
            }
            Stmt::Fn { name, params, ret, body } => {
                let (param_types, mut ret_ty) = fn_seeds
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| {
                        let pts = params
                            .iter()
                            .map(|(_, pty)| pty.clone().unwrap_or_else(|| state.fresh_var()))
                            .collect::<Vec<_>>();
                        let r = ret.clone().unwrap_or_else(|| state.fresh_var());
                        (pts, r)
                    });
                let mut env_body = env.clone();
                for ((pname, _), pt) in params.iter().zip(param_types.iter()) {
                    env_body.schemes.insert(pname.clone(), Scheme { vars: vec![], ty: pt.clone() });
                }
                let provisional = Type::Fun(param_types.clone(), Box::new(ret_ty.clone()));
                env_body.schemes.insert(name.clone(), Scheme { vars: vec![], ty: provisional.clone() });

                let (body_ty, s_body) = infer_expr(body, &env_body, &mut state, source)?;
                let body_ty = s_body.apply(&body_ty);
                if let Some(r) = ret {
                    let ann = s_body.apply(r);
                    unify(&body_ty, &ann, &body.span, source)?;
                    ret_ty = ann;
                } else {
                    ret_ty = body_ty;
                }
                let fn_ty = Type::Fun(
                    param_types.into_iter().map(|p| s_body.apply(&p)).collect(),
                    Box::new(ret_ty.clone()),
                );
                env.schemes.insert(name.clone(), generalize(&apply_env(&env, &s_body), &fn_ty));
                crate::debug::log_debug(debug.as_deref_mut(), || format!("fn {name} inferred as {:?}", fn_ty));
            }
            Stmt::Expr(expr) => {
                let _ = infer_expr(expr, &env, &mut state, source)?;
            }
        }
    }

    Ok(env)
}

fn infer_expr(
    expr: &Expr,
    env: &TypeEnv,
    state: &mut InferState,
    source: &str,
) -> Result<(Type, Subst), ParseError> {
    match &expr.kind {
        ExprKind::Number(_) => Ok((Type::Number, Subst::new())),
        ExprKind::Bool(_) => Ok((Type::Bool, Subst::new())),
        ExprKind::Str(_) => Ok((Type::String, Subst::new())),
        ExprKind::Var { name } => {
            if let Some(scheme) = env.schemes.get(name) {
                Ok((instantiate(scheme, state), Subst::new()))
            } else {
                Err(ParseError {
                    message: format!("unbound variable '{}'", name),
                    span: expr.span.clone(),
                    source: source.to_string(),
                })
            }
        }
        ExprKind::Binary { op, left, right } => {
            let (lt, ls) = infer_expr(left, env, state, source)?;
            let env_l = apply_env(env, &ls);
            let (rt, rs) = infer_expr(right, &env_l, state, source)?;
            let s = ls.compose(&rs);
            let lt = s.apply(&lt);
            let rt = s.apply(&rt);
            match op {
                crate::ast::BinOp::Add => {
                    let num = Type::Number;
                    let str_t = Type::String;
                    if let Ok(s1) = unify(&lt, &num, &expr.span, source) {
                        let s2 = unify(&rt, &num, &expr.span, source)?;
                        Ok((Type::Number, s.compose(&s1).compose(&s2)))
                    } else if let Ok(s1) = unify(&lt, &str_t, &expr.span, source) {
                        let s2 = unify(&rt, &str_t, &expr.span, source)?;
                        Ok((Type::String, s.compose(&s1).compose(&s2)))
                    } else {
                        Err(ParseError {
                            message: "type mismatch for '+'".into(),
                            span: expr.span.clone(),
                            source: source.to_string(),
                        })
                    }
                }
                crate::ast::BinOp::Sub | crate::ast::BinOp::Mul | crate::ast::BinOp::Div => {
                    let num = Type::Number;
                    let s1 = unify(&lt, &num, &expr.span, source)?;
                    let s2 = unify(&rt, &num, &expr.span, source)?;
                    Ok((Type::Number, s.compose(&s1).compose(&s2)))
                }
                crate::ast::BinOp::Eq | crate::ast::BinOp::Ne => {
                    let s1 = unify(&lt, &rt, &expr.span, source)?;
                    Ok((Type::Bool, s.compose(&s1)))
                }
                crate::ast::BinOp::Lt
                | crate::ast::BinOp::Gt
                | crate::ast::BinOp::Le
                | crate::ast::BinOp::Ge => {
                    let num = Type::Number;
                    let s1 = unify(&lt, &num, &expr.span, source)?;
                    let s2 = unify(&rt, &num, &expr.span, source)?;
                    Ok((Type::Bool, s.compose(&s1).compose(&s2)))
                }
                crate::ast::BinOp::And | crate::ast::BinOp::Or => {
                    let b = Type::Bool;
                    let s1 = unify(&lt, &b, &expr.span, source)?;
                    let s2 = unify(&rt, &b, &expr.span, source)?;
                    Ok((Type::Bool, s.compose(&s1).compose(&s2)))
                }
            }
        }
        ExprKind::Call { callee, args, callee_span } => {
            let mut s = Subst::new();
            let mut arg_types = Vec::new();
            for arg in args {
                let (t, st) = infer_expr(arg, &apply_env(env, &s), state, source)?;
                s = s.compose(&st);
                arg_types.push(s.apply(&t));
            }
            let ret_ty = state.fresh_var();
            let fn_ty = Type::Fun(arg_types.clone(), Box::new(ret_ty.clone()));
            let callee_ty = if let Some(sch) = env.schemes.get(callee) {
                instantiate(sch, state)
            } else {
                return Err(ParseError {
                    message: format!("unknown function '{}'", callee),
                    span: callee_span.clone(),
                    source: source.to_string(),
                });
            };
            let su = unify(&s.apply(&callee_ty), &fn_ty, &expr.span, source)?;
            s = s.compose(&su);
            Ok((s.apply(&ret_ty), s))
        }
        ExprKind::Lambda { params, body } => {
            let mut s = Subst::new();
            let mut env_fn = env.clone();
            let mut param_types = Vec::new();
            for (pname, pty) in params {
                let pt = if let Some(t) = pty {
                    s.apply(t)
                } else {
                    state.fresh_var()
                };
                param_types.push(pt.clone());
                env_fn.schemes.insert(pname.clone(), Scheme { vars: vec![], ty: pt });
            }
            let (body_ty, sb) = infer_expr(body, &env_fn, state, source)?;
            s = s.compose(&sb);
            let body_ty = s.apply(&body_ty);
            let fn_ty = Type::Fun(param_types.into_iter().map(|p| s.apply(&p)).collect(), Box::new(body_ty));
            Ok((fn_ty, s))
        }
        ExprKind::Block(stmts) => infer_block(stmts, env, state, source, &expr.span),
        ExprKind::Match { expr, arms } => infer_match(expr, arms, env, state, source, &expr.span),
    }
}

fn infer_block(
    stmts: &[Stmt],
    env: &TypeEnv,
    state: &mut InferState,
    source: &str,
    span: &Range<usize>,
) -> Result<(Type, Subst), ParseError> {
    if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
        return Err(ParseError {
            message: "block must end with expression".into(),
            span: span.clone(),
            source: source.to_string(),
        });
    }
    let mut s = Subst::new();
    let mut local_env = env.clone();

    let mut fn_seeds: HashMap<String, (Vec<Type>, Type)> = HashMap::new();
    for stmt in stmts {
        if let Stmt::Fn { name, params, ret, .. } = stmt {
            let mut param_types = Vec::new();
            for (_, pty) in params {
                let pt = if let Some(t) = pty {
                    s.apply(t)
                } else {
                    state.fresh_var()
                };
                param_types.push(pt);
            }
            let ret_ty = ret.as_ref().map(|t| s.apply(t)).unwrap_or_else(|| state.fresh_var());
            let fn_ty = Type::Fun(param_types.clone(), Box::new(ret_ty.clone()));
            local_env.schemes.insert(name.clone(), Scheme { vars: vec![], ty: fn_ty });
            fn_seeds.insert(name.clone(), (param_types, ret_ty));
        }
    }

    let mut last_ty = None;
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, ty, expr } => {
                let (t, st) = infer_expr(expr, &apply_env(&local_env, &s), state, source)?;
                s = s.compose(&st);
                let t_app = s.apply(&t);
                if let Some(ann) = ty {
                    let ann_t = s.apply(ann);
                    unify(&t_app, &ann_t, &expr.span, source)?;
                    local_env
                        .schemes
                        .insert(name.clone(), generalize(&apply_env(&local_env, &s), &ann_t));
                } else {
                    local_env
                        .schemes
                        .insert(name.clone(), generalize(&apply_env(&local_env, &s), &t_app));
                }
            }
            Stmt::Fn { name, params, ret, body } => {
                let mut env_fn = apply_env(&local_env, &s);
                let (param_types, mut ret_ty) = fn_seeds
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| {
                        let pts = params
                            .iter()
                            .map(|(_, pty)| pty.as_ref().map(|t| s.apply(t)).unwrap_or_else(|| state.fresh_var()))
                            .collect::<Vec<_>>();
                        let r = ret.as_ref().map(|t| s.apply(t)).unwrap_or_else(|| state.fresh_var());
                        (pts, r)
                    });
                for ((pname, _), pt) in params.iter().zip(param_types.iter()) {
                    env_fn.schemes.insert(pname.clone(), Scheme { vars: vec![], ty: pt.clone() });
                }
                let provisional = Type::Fun(param_types.clone(), Box::new(ret_ty.clone()));
                env_fn.schemes.insert(name.clone(), Scheme { vars: vec![], ty: provisional });

                let (body_ty, sb) = infer_expr(body, &env_fn, state, source)?;
                s = s.compose(&sb);
                let body_ty = s.apply(&body_ty);
                if let Some(r) = ret {
                    let ann = s.apply(r);
                    unify(&body_ty, &ann, &body.span, source)?;
                    ret_ty = ann;
                } else {
                    ret_ty = body_ty.clone();
                }
                let fn_ty = Type::Fun(
                    param_types.into_iter().map(|p| s.apply(&p)).collect(),
                    Box::new(ret_ty),
                );
                local_env
                    .schemes
                    .insert(name.clone(), generalize(&apply_env(&local_env, &s), &fn_ty));
            }
            Stmt::Expr(expr) => {
                let (t, st) = infer_expr(expr, &apply_env(&local_env, &s), state, source)?;
                s = s.compose(&st);
                last_ty = Some(s.apply(&t));
            }
        }
    }
    last_ty
        .map(|t| (t, s))
        .ok_or_else(|| ParseError {
            message: "block must end with expression".into(),
            span: span.clone(),
            source: source.to_string(),
        })
}

fn infer_match(
    expr: &Expr,
    arms: &[MatchArm],
    env: &TypeEnv,
    state: &mut InferState,
    source: &str,
    span: &Range<usize>,
) -> Result<(Type, Subst), ParseError> {
    if arms.is_empty() {
        return Err(ParseError {
            message: "match requires at least one arm".into(),
            span: span.clone(),
            source: source.to_string(),
        });
    }
    let (scrut_ty, s_scrut) = infer_expr(expr, env, state, source)?;
    let mut s = s_scrut;
    let mut result_ty = None;
    for arm in arms {
        let env_arm = apply_env(env, &s);
        let scrut = s.apply(&scrut_ty);
        if let Some(pat_ty) = pattern_expected_type(&arm.pat) {
            let su = unify(&scrut, &pat_ty, span, source)?;
            s = s.compose(&su);
        }
        let (arm_ty, sa) = infer_expr(&arm.expr, &env_arm, state, source)?;
        s = s.compose(&sa);
        let arm_ty = s.apply(&arm_ty);
        if let Some(prev) = result_ty {
            let su = unify(&prev, &arm_ty, &arm.expr.span, source)?;
            s = s.compose(&su);
            result_ty = Some(s.apply(&prev));
        } else {
            result_ty = Some(arm_ty);
        }
    }
    result_ty
        .map(|t| (t, s))
        .ok_or_else(|| ParseError {
            message: "match requires at least one arm".into(),
            span: span.clone(),
            source: source.to_string(),
        })
}

fn pattern_expected_type(pat: &Pattern) -> Option<Type> {
    match pat {
        Pattern::Wildcard => None,
        Pattern::Bool(_) => Some(Type::Bool),
        Pattern::Number(_) => Some(Type::Number),
        Pattern::Str(_) => Some(Type::String),
    }
}

fn apply_env(env: &TypeEnv, s: &Subst) -> TypeEnv {
    let mut new_env = env.clone();
    for scheme in new_env.schemes.values_mut() {
        scheme.ty = s.apply(&scheme.ty);
    }
    new_env
}

fn unify(a: &Type, b: &Type, span: &Range<usize>, source: &str) -> Result<Subst, ParseError> {
    match (a, b) {
        (Type::Fun(pa, ra), Type::Fun(pb, rb)) => {
            if pa.len() != pb.len() {
                return Err(ParseError {
                    message: "arity mismatch in function type".into(),
                    span: span.clone(),
                    source: source.to_string(),
                });
            }
            let mut subst = Subst::new();
            for (ta, tb) in pa.iter().zip(pb.iter()) {
                let su = unify(&subst.apply(ta), &subst.apply(tb), span, source)?;
                subst = subst.compose(&su);
            }
            let su = unify(&subst.apply(ra), &subst.apply(rb), span, source)?;
            Ok(subst.compose(&su))
        }
        (Type::Var(v), t) => bind(v, t, span, source),
        (t, Type::Var(v)) => bind(v, t, span, source),
        (Type::Number, Type::Number) => Ok(Subst::new()),
        (Type::String, Type::String) => Ok(Subst::new()),
        (Type::Bool, Type::Bool) => Ok(Subst::new()),
        _ => Err(ParseError {
            message: format!("type mismatch: {:?} vs {:?}", a, b),
            span: span.clone(),
            source: source.to_string(),
        }),
    }
}

fn bind(var: &TypeVarId, t: &Type, span: &Range<usize>, source: &str) -> Result<Subst, ParseError> {
    if let Type::Var(v2) = t {
        if var == v2 {
            return Ok(Subst::new());
        }
    }
    if occurs(var, t) {
        return Err(ParseError {
            message: "occurs check failed".into(),
            span: span.clone(),
            source: source.to_string(),
        });
    }
    let mut s = Subst::new();
    s.0.insert(var.clone(), t.clone());
    Ok(s)
}

fn occurs(var: &TypeVarId, t: &Type) -> bool {
    match t {
        Type::Var(v) => v == var,
        Type::Fun(ps, r) => ps.iter().any(|p| occurs(var, p)) || occurs(var, r),
        _ => false,
    }
}
