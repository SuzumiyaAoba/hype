use crate::ast::{Stmt, Type, Variant};

use super::super::parser::Sexp;
use super::super::transform_type::parse_type;
use super::Transformer;

impl Transformer {
    pub(super) fn transform_defn(&mut self, items: &[Sexp]) -> Result<Stmt, String> {
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
            Sexp::Vector(params) => self.parse_params(params)?,
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

    pub(super) fn parse_params(
        &self,
        params: &[Sexp],
    ) -> Result<Vec<(String, Option<Type>)>, String> {
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

    pub(super) fn transform_deftype(&mut self, items: &[Sexp]) -> Result<Stmt, String> {
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

        Ok(Stmt::TypeDecl {
            name,
            params,
            variants,
        })
    }

    pub(super) fn parse_variant(&self, sexp: &Sexp) -> Result<Variant, String> {
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
                                    _ => {
                                        return Err(
                                            "Variant field name must be a symbol".to_string()
                                        );
                                    }
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

    pub(super) fn transform_import(&mut self, items: &[Sexp]) -> Result<Stmt, String> {
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

    pub(super) fn transform_external(&mut self, items: &[Sexp]) -> Result<Stmt, String> {
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
}
