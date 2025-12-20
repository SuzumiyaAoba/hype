mod infer;
mod subst;

use std::collections::HashMap;

use crate::ast::{Scheme, Stmt, Type, TypeVarId};
use crate::debug::DebugInfo;
use crate::error::ParseError;

use infer::{InferState, infer_expr};
use subst::{apply_env, generalize, unify};

#[derive(Debug, Clone)]
pub struct TypeDef {
    #[allow(dead_code)]
    pub name: String,
    pub params: Vec<String>,
    pub variants: HashMap<String, Vec<(String, Type)>>,
}

#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    pub schemes: HashMap<String, Scheme>,
    pub type_defs: HashMap<String, TypeDef>,
}

pub fn type_name(t: &Type) -> String {
    match t {
        Type::Unit => "Unit".into(),
        Type::Number => "Number".into(),
        Type::String => "String".into(),
        Type::Bool => "Bool".into(),
        Type::Var(tv) => format!("t{}", tv.0),
        Type::Tuple(items) => {
            let inner: Vec<String> = items.iter().map(type_name).collect();
            format!("({})", inner.join(", "))
        }
        Type::List(inner) => format!("[{}]", type_name(inner)),
        Type::Fun(params, ret) => {
            let ps: Vec<String> = params.iter().map(type_name).collect();
            format!("({}) -> {}", ps.join(", "), type_name(ret))
        }
        Type::Adt { name, args } => {
            if args.is_empty() {
                name.clone()
            } else {
                let args_str: Vec<String> = args.iter().map(type_name).collect();
                format!("{}<{}>", name, args_str.join(", "))
            }
        }
        Type::Record(fields) => {
            let fields_str: Vec<String> = fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, type_name(ty)))
                .collect();
            format!("{{ {} }}", fields_str.join(", "))
        }
    }
}

fn prelude_env() -> TypeEnv {
    let mut env = TypeEnv::default();
    let tv = TypeVarId(0);
    let fix_ty = Type::Fun(
        vec![Type::Fun(
            vec![Type::Var(tv.clone())],
            Box::new(Type::Var(tv.clone())),
        )],
        Box::new(Type::Var(tv.clone())),
    );
    env.schemes.insert("fix".to_string(), Scheme {
        vars: vec![tv],
        ty: fix_ty,
    });
    env
}

pub(crate) fn typecheck(
    stmts: &[Stmt],
    source: &str,
    mut debug: Option<&mut DebugInfo>,
) -> Result<TypeEnv, ParseError> {
    let mut env = prelude_env();
    let mut state = InferState::default();

    let mut fn_seeds: HashMap<String, (Vec<Type>, Type)> = HashMap::new();
    for stmt in stmts {
        if let Stmt::Fn {
            name, params, ret, ..
        } = stmt
        {
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
            env.schemes.insert(name.clone(), Scheme {
                vars: vec![],
                ty: fn_ty,
            });
            fn_seeds.insert(name.clone(), (param_types, ret_ty));
        }
    }

    for stmt in stmts {
        match stmt {
            Stmt::Let {
                name,
                ty,
                expr,
                recursive,
            } => {
                let mut env_for_infer = env.clone();
                if *recursive {
                    let seed = ty.clone().unwrap_or_else(|| state.fresh_var());
                    env_for_infer.schemes.insert(name.clone(), Scheme {
                        vars: vec![],
                        ty: seed,
                    });
                }
                let (t, s) = infer_expr(expr, &env_for_infer, &mut state, source)?;
                let t_app = s.apply(&t);
                if let Some(annot) = ty {
                    let ann = s.apply(annot);
                    unify(&t_app, &ann, &expr.span, source)?;
                    env.schemes
                        .insert(name.clone(), generalize(&apply_env(&env, &s), &ann));
                    crate::debug::log_debug(debug.as_deref_mut(), || {
                        format!("let {name} annotated as {:?}", ann)
                    });
                } else {
                    env.schemes
                        .insert(name.clone(), generalize(&apply_env(&env, &s), &t_app));
                    crate::debug::log_debug(debug.as_deref_mut(), || {
                        format!("let {name} inferred as {:?}", t_app)
                    });
                }
            }
            Stmt::Fn {
                name,
                params,
                ret,
                body,
            } => {
                let (param_types, mut ret_ty) = fn_seeds.get(name).cloned().unwrap_or_else(|| {
                    let pts = params
                        .iter()
                        .map(|(_, pty)| pty.clone().unwrap_or_else(|| state.fresh_var()))
                        .collect::<Vec<_>>();
                    let r = ret.clone().unwrap_or_else(|| state.fresh_var());
                    (pts, r)
                });
                let mut env_body = env.clone();
                for ((pname, _), pt) in params.iter().zip(param_types.iter()) {
                    env_body.schemes.insert(pname.clone(), Scheme {
                        vars: vec![],
                        ty: pt.clone(),
                    });
                }
                let provisional = Type::Fun(param_types.clone(), Box::new(ret_ty.clone()));
                env_body.schemes.insert(name.clone(), Scheme {
                    vars: vec![],
                    ty: provisional.clone(),
                });

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
                env.schemes
                    .insert(name.clone(), generalize(&apply_env(&env, &s_body), &fn_ty));
                crate::debug::log_debug(debug.as_deref_mut(), || {
                    format!("fn {name} inferred as {:?}", fn_ty)
                });
            }
            Stmt::External { name, ty, .. } => {
                env.schemes.insert(name.clone(), Scheme {
                    vars: vec![],
                    ty: ty.clone(),
                });
                crate::debug::log_debug(debug.as_deref_mut(), || {
                    format!("external {name} declared as {:?}", ty)
                });
            }
            Stmt::Expr(expr) => {
                let _ = infer_expr(expr, &env, &mut state, source)?;
            }
            Stmt::Import { .. } => {
                // Import statements are resolved before type checking
            }
            Stmt::TypeDecl {
                name,
                params,
                variants,
            } => {
                // Register type definition
                let mut variant_map = HashMap::new();
                for v in variants {
                    variant_map.insert(v.name.clone(), v.fields.clone());
                }
                let type_def = TypeDef {
                    name: name.clone(),
                    params: params.clone(),
                    variants: variant_map,
                };
                env.type_defs.insert(name.clone(), type_def);

                // Register constructors as functions
                for v in variants {
                    let result_ty = if params.is_empty() {
                        Type::Adt {
                            name: name.clone(),
                            args: vec![],
                        }
                    } else {
                        Type::Adt {
                            name: name.clone(),
                            args: params
                                .iter()
                                .map(|p| Type::Adt {
                                    name: p.clone(),
                                    args: vec![],
                                })
                                .collect(),
                        }
                    };

                    let ctor_ty = if v.fields.is_empty() {
                        result_ty.clone()
                    } else {
                        let field_types: Vec<Type> =
                            v.fields.iter().map(|(_, t)| t.clone()).collect();
                        Type::Fun(field_types, Box::new(result_ty.clone()))
                    };

                    env.schemes.insert(v.name.clone(), Scheme {
                        vars: vec![],
                        ty: ctor_ty,
                    });
                }
            }
        }
    }

    Ok(env)
}
