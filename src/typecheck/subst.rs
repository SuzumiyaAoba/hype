use std::collections::{HashMap, HashSet};
use std::ops::Range;

use crate::ast::{Scheme, Type, TypeVarId};
use crate::error::ParseError;

use super::TypeEnv;

#[derive(Debug, Clone, Default)]
pub(super) struct Subst(pub HashMap<TypeVarId, Type>);

impl Subst {
    pub fn new() -> Self {
        Subst(HashMap::new())
    }

    pub fn apply(&self, t: &Type) -> Type {
        match t {
            Type::Var(tv) => self.0.get(tv).cloned().unwrap_or(Type::Var(tv.clone())),
            Type::Tuple(items) => Type::Tuple(items.iter().map(|i| self.apply(i)).collect()),
            Type::List(inner) => Type::List(Box::new(self.apply(inner))),
            Type::Fun(ps, r) => Type::Fun(
                ps.iter().map(|p| self.apply(p)).collect(),
                Box::new(self.apply(r)),
            ),
            Type::Adt { name, args } => Type::Adt {
                name: name.clone(),
                args: args.iter().map(|a| self.apply(a)).collect(),
            },
            Type::Record(fields) => Type::Record(
                fields
                    .iter()
                    .map(|(n, t)| (n.clone(), self.apply(t)))
                    .collect(),
            ),
            other => other.clone(),
        }
    }

    pub fn compose(&self, other: &Subst) -> Subst {
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

pub(super) fn free_type_vars(t: &Type, acc: &mut HashSet<TypeVarId>) {
    match t {
        Type::Var(tv) => {
            acc.insert(tv.clone());
        }
        Type::Tuple(items) => {
            for it in items {
                free_type_vars(it, acc);
            }
        }
        Type::List(inner) => free_type_vars(inner, acc),
        Type::Fun(ps, r) => {
            for p in ps {
                free_type_vars(p, acc);
            }
            free_type_vars(r, acc);
        }
        Type::Adt { args, .. } => {
            for arg in args {
                free_type_vars(arg, acc);
            }
        }
        Type::Record(fields) => {
            for (_, ty) in fields {
                free_type_vars(ty, acc);
            }
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

pub(super) fn free_vars_env(env: &TypeEnv) -> HashSet<TypeVarId> {
    let mut set = HashSet::new();
    for scheme in env.schemes.values() {
        set.extend(free_vars_scheme(scheme));
    }
    set
}

pub(super) fn generalize(env: &TypeEnv, t: &Type) -> Scheme {
    let mut ftv = HashSet::new();
    free_type_vars(t, &mut ftv);
    let env_ftv = free_vars_env(env);
    ftv.retain(|v| !env_ftv.contains(v));
    Scheme {
        vars: ftv.into_iter().collect(),
        ty: t.clone(),
    }
}

pub(super) fn apply_env(env: &TypeEnv, s: &Subst) -> TypeEnv {
    let mut new_env = env.clone();
    for scheme in new_env.schemes.values_mut() {
        scheme.ty = s.apply(&scheme.ty);
    }
    new_env
}

pub(super) fn unify(
    a: &Type,
    b: &Type,
    span: &Range<usize>,
    source: &str,
) -> Result<Subst, ParseError> {
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
        (Type::Tuple(ae), Type::Tuple(be)) => {
            if ae.len() != be.len() {
                return Err(ParseError {
                    message: "arity mismatch in tuple type".into(),
                    span: span.clone(),
                    source: source.to_string(),
                });
            }
            let mut subst = Subst::new();
            for (ta, tb) in ae.iter().zip(be.iter()) {
                let su = unify(&subst.apply(ta), &subst.apply(tb), span, source)?;
                subst = subst.compose(&su);
            }
            Ok(subst)
        }
        (Type::List(a), Type::List(b)) => unify(a, b, span, source),
        (Type::Adt { name: na, args: aa }, Type::Adt { name: nb, args: ab }) => {
            if na != nb {
                return Err(ParseError {
                    message: format!("type mismatch: {} vs {}", na, nb),
                    span: span.clone(),
                    source: source.to_string(),
                });
            }
            if aa.len() != ab.len() {
                return Err(ParseError {
                    message: format!("type argument count mismatch for {}", na),
                    span: span.clone(),
                    source: source.to_string(),
                });
            }
            let mut subst = Subst::new();
            for (ta, tb) in aa.iter().zip(ab.iter()) {
                let su = unify(&subst.apply(ta), &subst.apply(tb), span, source)?;
                subst = subst.compose(&su);
            }
            Ok(subst)
        }
        (Type::Var(v), t) => bind(v, t, span, source),
        (t, Type::Var(v)) => bind(v, t, span, source),
        (Type::Unit, Type::Unit) => Ok(Subst::new()),
        (Type::Number, Type::Number) => Ok(Subst::new()),
        (Type::String, Type::String) => Ok(Subst::new()),
        (Type::Bool, Type::Bool) => Ok(Subst::new()),
        (Type::Record(fa), Type::Record(fb)) => {
            if fa.len() != fb.len() {
                return Err(ParseError {
                    message: "record field count mismatch".into(),
                    span: span.clone(),
                    source: source.to_string(),
                });
            }
            let mut subst = Subst::new();
            for ((na, ta), (nb, tb)) in fa.iter().zip(fb.iter()) {
                if na != nb {
                    return Err(ParseError {
                        message: format!("record field name mismatch: {} vs {}", na, nb),
                        span: span.clone(),
                        source: source.to_string(),
                    });
                }
                let su = unify(&subst.apply(ta), &subst.apply(tb), span, source)?;
                subst = subst.compose(&su);
            }
            Ok(subst)
        }
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
        Type::Tuple(items) => items.iter().any(|p| occurs(var, p)),
        Type::List(inner) => occurs(var, inner),
        Type::Fun(ps, r) => ps.iter().any(|p| occurs(var, p)) || occurs(var, r),
        Type::Adt { args, .. } => args.iter().any(|a| occurs(var, a)),
        Type::Record(fields) => fields.iter().any(|(_, ty)| occurs(var, ty)),
        _ => false,
    }
}
