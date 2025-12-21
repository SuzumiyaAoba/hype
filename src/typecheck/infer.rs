use std::collections::HashMap;
use std::ops::Range;

use crate::ast::{Expr, ExprKind, MatchArm, Pattern, Scheme, Stmt, Type, TypeVarId};
use crate::error::ParseError;

use super::TypeEnv;
use super::subst::{Subst, apply_env, generalize, unify};

#[derive(Default)]
pub(super) struct InferState {
    counter: u32,
}

impl InferState {
    pub fn fresh_var(&mut self) -> Type {
        let id = self.counter;
        self.counter += 1;
        Type::Var(TypeVarId(id))
    }
}

pub(super) fn instantiate(s: &Scheme, state: &mut InferState) -> Type {
    let mut subst = Subst::new();
    for v in &s.vars {
        subst.0.insert(v.clone(), state.fresh_var());
    }
    subst.apply(&s.ty)
}

pub(super) fn infer_expr(
    expr: &Expr,
    env: &TypeEnv,
    state: &mut InferState,
    source: &str,
) -> Result<(Type, Subst), ParseError> {
    match &expr.kind {
        ExprKind::Number(_) => Ok((Type::Number, Subst::new())),
        ExprKind::Bool(_) => Ok((Type::Bool, Subst::new())),
        ExprKind::Str(_) => Ok((Type::String, Subst::new())),
        ExprKind::Tuple(items) => {
            let mut s = Subst::new();
            let mut tys = Vec::new();
            for it in items {
                let (t, st) = infer_expr(it, &apply_env(env, &s), state, source)?;
                s = s.compose(&st);
                tys.push(s.apply(&t));
            }
            Ok((Type::Tuple(tys), s))
        }
        ExprKind::List(items) => {
            let mut s = Subst::new();
            let mut elem_ty = None;
            for it in items {
                let (t, st) = infer_expr(it, &apply_env(env, &s), state, source)?;
                s = s.compose(&st);
                let t_app = s.apply(&t);
                if let Some(et) = elem_ty {
                    let su = unify(&et, &t_app, &expr.span, source)?;
                    s = s.compose(&su);
                    elem_ty = Some(s.apply(&et));
                } else {
                    elem_ty = Some(t_app);
                }
            }
            let elem = elem_ty.unwrap_or_else(|| state.fresh_var());
            Ok((Type::List(Box::new(elem)), s))
        }
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
                crate::ast::BinOp::Sub | crate::ast::BinOp::Mul | crate::ast::BinOp::Div | crate::ast::BinOp::Mod => {
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
        ExprKind::Call { callee, args } => {
            let mut s = Subst::new();
            // Infer callee type
            let (callee_ty, st) = infer_expr(callee, env, state, source)?;
            s = s.compose(&st);
            // Infer argument types
            let mut arg_types = Vec::new();
            for arg in args {
                let (t, st) = infer_expr(arg, &apply_env(env, &s), state, source)?;
                s = s.compose(&st);
                arg_types.push(s.apply(&t));
            }
            let ret_ty = state.fresh_var();
            let fn_ty = Type::Fun(arg_types.clone(), Box::new(ret_ty.clone()));
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
                env_fn.schemes.insert(pname.clone(), Scheme {
                    vars: vec![],
                    ty: pt,
                });
            }
            let (body_ty, sb) = infer_expr(body, &env_fn, state, source)?;
            s = s.compose(&sb);
            let body_ty = s.apply(&body_ty);
            let fn_ty = Type::Fun(
                param_types.into_iter().map(|p| s.apply(&p)).collect(),
                Box::new(body_ty),
            );
            Ok((fn_ty, s))
        }
        ExprKind::Block(stmts) => infer_block(stmts, env, state, source, &expr.span),
        ExprKind::Match { expr, arms } => infer_match(expr, arms, env, state, source, &expr.span),
        ExprKind::Constructor { name, fields } => {
            // Look up constructor in type_defs
            let (type_name, type_params, field_types) =
                find_constructor(env, name).ok_or_else(|| ParseError {
                    message: format!("unknown constructor `{}`", name),
                    span: expr.span.clone(),
                    source: source.to_string(),
                })?;

            let mut s = Subst::new();

            // Create fresh type variables for each type parameter
            let mut param_map: HashMap<String, Type> = HashMap::new();
            let mut type_args: Vec<Type> = Vec::new();
            for param in &type_params {
                let tv = state.fresh_var();
                param_map.insert(param.clone(), tv.clone());
                type_args.push(tv);
            }

            // Check that all required fields are provided
            if fields.len() != field_types.len() {
                return Err(ParseError {
                    message: format!(
                        "constructor `{}` expects {} fields, got {}",
                        name,
                        field_types.len(),
                        fields.len()
                    ),
                    span: expr.span.clone(),
                    source: source.to_string(),
                });
            }

            // Type check each field with substituted type params
            for (field_name, field_expr) in fields {
                let expected_ty = field_types
                    .iter()
                    .find(|(n, _)| n == field_name)
                    .map(|(_, t)| substitute_type_params(t, &param_map))
                    .ok_or_else(|| ParseError {
                        message: format!(
                            "unknown field `{}` in constructor `{}`",
                            field_name, name
                        ),
                        span: field_expr.span.clone(),
                        source: source.to_string(),
                    })?;

                let (actual_ty, field_s) =
                    infer_expr(field_expr, &apply_env(env, &s), state, source)?;
                s = s.compose(&field_s);
                unify(
                    &s.apply(&expected_ty),
                    &s.apply(&actual_ty),
                    &field_expr.span,
                    source,
                )?;
            }

            let result_ty = Type::Adt {
                name: type_name,
                args: type_args.iter().map(|a| s.apply(a)).collect(),
            };
            Ok((result_ty, s))
        }
        ExprKind::Record(fields) => {
            let mut s = Subst::new();
            let mut field_types = Vec::new();
            for (field_name, field_expr) in fields {
                let (ty, field_s) = infer_expr(field_expr, &apply_env(env, &s), state, source)?;
                s = s.compose(&field_s);
                field_types.push((field_name.clone(), s.apply(&ty)));
            }
            Ok((Type::Record(field_types), s))
        }
        ExprKind::FieldAccess {
            expr: record_expr,
            field,
        } => {
            let (record_ty, s) = infer_expr(record_expr, env, state, source)?;
            let record_ty = s.apply(&record_ty);

            // Try to find the field in the record type
            match &record_ty {
                Type::Record(fields) => {
                    if let Some((_, field_ty)) = fields.iter().find(|(n, _)| n == field) {
                        Ok((field_ty.clone(), s))
                    } else {
                        Err(ParseError {
                            message: format!("no field `{}` in record type", field),
                            span: expr.span.clone(),
                            source: source.to_string(),
                        })
                    }
                }
                Type::Var(_) => {
                    // Unknown record type, create a fresh type variable for the field
                    let field_ty = state.fresh_var();
                    Ok((field_ty, s))
                }
                _ => Err(ParseError {
                    message: format!("cannot access field `{}` on non-record type", field),
                    span: expr.span.clone(),
                    source: source.to_string(),
                }),
            }
        }
    }
}

fn find_constructor(
    env: &TypeEnv,
    ctor_name: &str,
) -> Option<(String, Vec<String>, Vec<(String, Type)>)> {
    for (type_name, type_def) in &env.type_defs {
        if let Some(fields) = type_def.variants.get(ctor_name) {
            return Some((type_name.clone(), type_def.params.clone(), fields.clone()));
        }
    }
    None
}

pub(super) fn substitute_type_params(ty: &Type, param_map: &HashMap<String, Type>) -> Type {
    match ty {
        Type::Adt { name, args } if args.is_empty() => {
            if let Some(replacement) = param_map.get(name) {
                replacement.clone()
            } else {
                ty.clone()
            }
        }
        Type::Adt { name, args } => Type::Adt {
            name: name.clone(),
            args: args
                .iter()
                .map(|a| substitute_type_params(a, param_map))
                .collect(),
        },
        Type::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|i| substitute_type_params(i, param_map))
                .collect(),
        ),
        Type::List(inner) => Type::List(Box::new(substitute_type_params(inner, param_map))),
        Type::Fun(ps, r) => Type::Fun(
            ps.iter()
                .map(|p| substitute_type_params(p, param_map))
                .collect(),
            Box::new(substitute_type_params(r, param_map)),
        ),
        _ => ty.clone(),
    }
}

pub(super) fn infer_block(
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
        if let Stmt::Fn {
            name, params, ret, ..
        } = stmt
        {
            let mut param_types = Vec::new();
            for (_, pty) in params {
                let pt = if let Some(t) = pty {
                    s.apply(t)
                } else {
                    state.fresh_var()
                };
                param_types.push(pt);
            }
            let ret_ty = ret
                .as_ref()
                .map(|t| s.apply(t))
                .unwrap_or_else(|| state.fresh_var());
            let fn_ty = Type::Fun(param_types.clone(), Box::new(ret_ty.clone()));
            local_env.schemes.insert(name.clone(), Scheme {
                vars: vec![],
                ty: fn_ty,
            });
            fn_seeds.insert(name.clone(), (param_types, ret_ty));
        }
    }

    let mut last_ty = None;
    for stmt in stmts {
        match stmt {
            Stmt::Let {
                name,
                ty,
                expr,
                recursive,
            } => {
                let mut env_infer = apply_env(&local_env, &s);
                if *recursive {
                    let seed = ty
                        .as_ref()
                        .map(|t| s.apply(t))
                        .unwrap_or_else(|| state.fresh_var());
                    env_infer.schemes.insert(name.clone(), Scheme {
                        vars: vec![],
                        ty: seed,
                    });
                }
                let (t, st) = infer_expr(expr, &env_infer, state, source)?;
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
            Stmt::Fn {
                name,
                params,
                ret,
                body,
            } => {
                let mut env_fn = apply_env(&local_env, &s);
                let (param_types, mut ret_ty) = fn_seeds.get(name).cloned().unwrap_or_else(|| {
                    let pts = params
                        .iter()
                        .map(|(_, pty)| {
                            pty.as_ref()
                                .map(|t| s.apply(t))
                                .unwrap_or_else(|| state.fresh_var())
                        })
                        .collect::<Vec<_>>();
                    let r = ret
                        .as_ref()
                        .map(|t| s.apply(t))
                        .unwrap_or_else(|| state.fresh_var());
                    (pts, r)
                });
                for ((pname, _), pt) in params.iter().zip(param_types.iter()) {
                    env_fn.schemes.insert(pname.clone(), Scheme {
                        vars: vec![],
                        ty: pt.clone(),
                    });
                }
                let provisional = Type::Fun(param_types.clone(), Box::new(ret_ty.clone()));
                env_fn.schemes.insert(name.clone(), Scheme {
                    vars: vec![],
                    ty: provisional,
                });

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
            Stmt::External { name, ty, .. } => {
                local_env.schemes.insert(name.clone(), Scheme {
                    vars: vec![],
                    ty: ty.clone(),
                });
            }
            Stmt::Expr(expr) => {
                let (t, st) = infer_expr(expr, &apply_env(&local_env, &s), state, source)?;
                s = s.compose(&st);
                last_ty = Some(s.apply(&t));
            }
            Stmt::Import { .. } => {
                // Import statements are resolved before type checking
            }
            Stmt::TypeDecl {
                name,
                params,
                variants,
            } => {
                // Register type definition in local environment
                let mut variant_map = HashMap::new();
                for v in variants {
                    variant_map.insert(v.name.clone(), v.fields.clone());
                }
                let type_def = super::TypeDef {
                    name: name.clone(),
                    params: params.clone(),
                    variants: variant_map,
                };
                local_env.type_defs.insert(name.clone(), type_def);

                // Register constructors
                for v in variants {
                    let result_ty = Type::Adt {
                        name: name.clone(),
                        args: vec![],
                    };
                    let ctor_ty = if v.fields.is_empty() {
                        result_ty.clone()
                    } else {
                        let field_types: Vec<Type> =
                            v.fields.iter().map(|(_, t)| t.clone()).collect();
                        Type::Fun(field_types, Box::new(result_ty))
                    };
                    local_env.schemes.insert(v.name.clone(), Scheme {
                        vars: vec![],
                        ty: ctor_ty,
                    });
                }
            }
        }
    }
    last_ty.map(|t| (t, s)).ok_or_else(|| ParseError {
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
        let mut env_arm = apply_env(env, &s);
        let scrut = s.apply(&scrut_ty);
        let (pat_ty, bindings) = pattern_info(&arm.pat, &env_arm, state);
        let su = unify(&scrut, &pat_ty, span, source)?;
        s = s.compose(&su);
        for (name, ty) in bindings {
            env_arm.schemes.insert(name, Scheme {
                vars: vec![],
                ty: s.apply(&ty),
            });
        }
        // Type check guard expression if present (must be Bool)
        if let Some(guard) = &arm.guard {
            let (guard_ty, sg) = infer_expr(guard, &env_arm, state, source)?;
            s = s.compose(&sg);
            let su = unify(&guard_ty, &Type::Bool, &guard.span, source)?;
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

    // Check exhaustiveness
    let final_scrut_ty = s.apply(&scrut_ty);
    let patterns: Vec<Pattern> = arms.iter().map(|a| a.pat.clone()).collect();
    let exhaustiveness = crate::exhaustive::check_exhaustiveness(&patterns, &final_scrut_ty, env);
    if let Some(warning) = crate::exhaustive::format_warning(&exhaustiveness) {
        eprintln!("warning: {}", warning);
    }

    result_ty.map(|t| (t, s)).ok_or_else(|| ParseError {
        message: "match requires at least one arm".into(),
        span: span.clone(),
        source: source.to_string(),
    })
}

fn pattern_info(
    pat: &Pattern,
    env: &TypeEnv,
    state: &mut InferState,
) -> (Type, Vec<(String, Type)>) {
    match pat {
        Pattern::Wildcard => (state.fresh_var(), Vec::new()),
        Pattern::Bind(name) => {
            let ty = state.fresh_var();
            (ty.clone(), vec![(name.clone(), ty)])
        }
        Pattern::Bool(_) => (Type::Bool, Vec::new()),
        Pattern::Number(_) => (Type::Number, Vec::new()),
        Pattern::Str(_) => (Type::String, Vec::new()),
        Pattern::Tuple(items) => {
            let mut binds = Vec::new();
            let elems = items
                .iter()
                .map(|p| {
                    let (t, b) = pattern_info(p, env, state);
                    binds.extend(b);
                    t
                })
                .collect();
            (Type::Tuple(elems), binds)
        }
        Pattern::List { items, rest } => {
            let mut binds = Vec::new();
            let elem_ty = if let Some(first) = items.first() {
                let (t, b) = pattern_info(first, env, state);
                binds.extend(b);
                t
            } else {
                state.fresh_var()
            };
            for p in items.iter().skip(1) {
                let (_t, b) = pattern_info(p, env, state);
                binds.extend(b);
            }
            if let Some(name) = rest {
                binds.push((name.clone(), Type::List(Box::new(elem_ty.clone()))));
            }
            (Type::List(Box::new(elem_ty)), binds)
        }
        Pattern::Constructor { name, fields } => {
            // Look up constructor to get the proper type
            if let Some((type_name, type_params, field_types)) = find_constructor(env, name) {
                let mut binds = Vec::new();

                // Create fresh type variables for type parameters
                let mut param_map: HashMap<String, Type> = HashMap::new();
                let mut type_args: Vec<Type> = Vec::new();
                for param in &type_params {
                    let tv = state.fresh_var();
                    param_map.insert(param.clone(), tv.clone());
                    type_args.push(tv);
                }

                // Process field patterns and collect bindings
                for (field_name, field_pat) in fields {
                    let (_, field_binds) = pattern_info(field_pat, env, state);
                    binds.extend(field_binds);

                    // Find the expected field type and substitute type params
                    if let Some((_, expected_ty)) =
                        field_types.iter().find(|(n, _)| n == field_name)
                    {
                        let expected_ty = substitute_type_params(expected_ty, &param_map);
                        // Add constraint that field pattern type should match expected
                        // This will be handled by unification later
                        if let Pattern::Bind(bind_name) = field_pat {
                            // Override the fresh type with the expected type
                            binds.retain(|(n, _)| n != bind_name);
                            binds.push((bind_name.clone(), expected_ty));
                        }
                    }
                }

                (
                    Type::Adt {
                        name: type_name,
                        args: type_args,
                    },
                    binds,
                )
            } else {
                // Constructor not found - return a fresh type variable and let error be caught later
                let mut binds = Vec::new();
                for (_, field_pat) in fields {
                    let (_, field_binds) = pattern_info(field_pat, env, state);
                    binds.extend(field_binds);
                }
                (state.fresh_var(), binds)
            }
        }
        Pattern::Record { fields } => {
            let mut binds = Vec::new();
            let mut field_types = Vec::new();
            for (field_name, field_pat) in fields {
                let (ty, field_binds) = pattern_info(field_pat, env, state);
                binds.extend(field_binds);
                field_types.push((field_name.clone(), ty));
            }
            (Type::Record(field_types), binds)
        }
    }
}
