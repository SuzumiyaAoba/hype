use crate::ast::{MatchArm, Pattern};

use super::super::parser::Sexp;
use super::Transformer;

impl Transformer {
    pub(super) fn parse_match_arm(&mut self, items: &[Sexp]) -> Result<MatchArm, String> {
        // [pattern body] or [pattern :when guard body]
        match items.len() {
            2 => {
                // No guard: [pattern body]
                let pat = self.parse_pattern(&items[0])?;
                let body = self.transform_expr(items[1].clone())?;
                Ok(MatchArm {
                    pat,
                    guard: None,
                    expr: body,
                })
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
                Ok(MatchArm {
                    pat,
                    guard: Some(guard),
                    expr: body,
                })
            }
            _ => Err("match arm must be [pattern body] or [pattern :when guard body]".to_string()),
        }
    }

    pub(super) fn parse_pattern(&self, sexp: &Sexp) -> Result<Pattern, String> {
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
                let patterns: Result<Vec<_>, _> =
                    items.iter().map(|item| self.parse_pattern(item)).collect();
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

                Ok(Pattern::List {
                    items: patterns,
                    rest,
                })
            }

            // Constructor pattern: (Name {:field pat ...}) or (Name)
            Sexp::List(items) => {
                if items.is_empty() {
                    return Err("Empty list is not a valid pattern".to_string());
                }

                match &items[0] {
                    Sexp::Symbol(name)
                        if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) =>
                    {
                        // Constructor pattern
                        let mut fields = Vec::new();

                        if items.len() > 1 {
                            // Parse fields from map: {:field pat ...}
                            match &items[1] {
                                Sexp::Map(pairs) => {
                                    for (key, value) in pairs {
                                        let field_name = match key {
                                            Sexp::Symbol(n) => n.clone(),
                                            _ => {
                                                return Err(
                                                    "Constructor field name must be a symbol"
                                                        .to_string(),
                                                );
                                            }
                                        };
                                        let field_pat = self.parse_pattern(value)?;
                                        fields.push((field_name, field_pat));
                                    }
                                }
                                _ => {
                                    return Err(
                                        "Constructor fields must be a map {:field pat}".to_string()
                                    );
                                }
                            }
                        }

                        Ok(Pattern::Constructor {
                            name: name.clone(),
                            fields,
                        })
                    }
                    _ => Err(
                        "Invalid pattern: list must start with uppercase constructor name"
                            .to_string(),
                    ),
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
