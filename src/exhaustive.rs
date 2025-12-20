//! Pattern exhaustiveness checking
//!
//! This module implements exhaustiveness checking for match expressions.
//! It determines whether a set of patterns covers all possible values of a type.

use std::collections::HashSet;

use crate::ast::{Pattern, Type};
use crate::typecheck::TypeEnv;

/// Result of exhaustiveness check
#[derive(Debug, Clone)]
pub struct ExhaustivenessResult {
    pub is_exhaustive: bool,
    pub missing_patterns: Vec<String>,
}

/// Check if a set of patterns is exhaustive for the given type
pub fn check_exhaustiveness(
    patterns: &[Pattern],
    scrutinee_type: &Type,
    env: &TypeEnv,
) -> ExhaustivenessResult {
    let missing = find_missing_patterns(patterns, scrutinee_type, env);
    ExhaustivenessResult {
        is_exhaustive: missing.is_empty(),
        missing_patterns: missing,
    }
}

/// Find patterns that are not covered
fn find_missing_patterns(patterns: &[Pattern], ty: &Type, env: &TypeEnv) -> Vec<String> {
    // Check if any pattern is a wildcard or bind (catches everything)
    if patterns.iter().any(|p| matches!(p, Pattern::Wildcard | Pattern::Bind(_))) {
        return vec![];
    }

    match ty {
        Type::Bool => check_bool_exhaustiveness(patterns),
        Type::Number => check_number_exhaustiveness(patterns),
        Type::String => check_string_exhaustiveness(patterns),
        Type::Tuple(elem_types) => check_tuple_exhaustiveness(patterns, elem_types, env),
        Type::List(elem_type) => check_list_exhaustiveness(patterns, elem_type, env),
        Type::Adt { name, .. } => check_adt_exhaustiveness(patterns, name, env),
        Type::Unit => vec![], // Unit has only one value
        Type::Var(_) => vec![], // Unknown type, assume exhaustive
        Type::Fun(_, _) => vec![], // Function types can't be pattern matched meaningfully
        Type::Record(_) => vec![], // Record exhaustiveness is complex, assume exhaustive for now
    }
}

/// Check Bool exhaustiveness: need both true and false
fn check_bool_exhaustiveness(patterns: &[Pattern]) -> Vec<String> {
    let mut has_true = false;
    let mut has_false = false;

    for pat in patterns {
        match pat {
            Pattern::Bool(true) => has_true = true,
            Pattern::Bool(false) => has_false = true,
            Pattern::Wildcard | Pattern::Bind(_) => {
                has_true = true;
                has_false = true;
            }
            _ => {}
        }
    }

    let mut missing = vec![];
    if !has_true {
        missing.push("true".to_string());
    }
    if !has_false {
        missing.push("false".to_string());
    }
    missing
}

/// Check Number exhaustiveness: need wildcard or bind
fn check_number_exhaustiveness(patterns: &[Pattern]) -> Vec<String> {
    // Numbers are infinite, so we need a catch-all pattern
    let has_catchall = patterns.iter().any(|p| {
        matches!(p, Pattern::Wildcard | Pattern::Bind(_))
    });

    if has_catchall {
        vec![]
    } else {
        vec!["_".to_string()]
    }
}

/// Check String exhaustiveness: need wildcard or bind
fn check_string_exhaustiveness(patterns: &[Pattern]) -> Vec<String> {
    // Strings are infinite, so we need a catch-all pattern
    let has_catchall = patterns.iter().any(|p| {
        matches!(p, Pattern::Wildcard | Pattern::Bind(_))
    });

    if has_catchall {
        vec![]
    } else {
        vec!["_".to_string()]
    }
}

/// Check Tuple exhaustiveness: recursively check each element
fn check_tuple_exhaustiveness(
    patterns: &[Pattern],
    elem_types: &[Type],
    env: &TypeEnv,
) -> Vec<String> {
    // If any pattern is a wildcard or bind, it's exhaustive
    if patterns.iter().any(|p| matches!(p, Pattern::Wildcard | Pattern::Bind(_))) {
        return vec![];
    }

    // Collect tuple patterns
    let tuple_patterns: Vec<&Vec<Pattern>> = patterns
        .iter()
        .filter_map(|p| match p {
            Pattern::Tuple(elems) if elems.len() == elem_types.len() => Some(elems),
            _ => None,
        })
        .collect();

    if tuple_patterns.is_empty() && !elem_types.is_empty() {
        return vec![format!("({})", vec!["_"; elem_types.len()].join(", "))];
    }

    // For each position, check exhaustiveness
    let mut all_missing = vec![];
    for (i, elem_ty) in elem_types.iter().enumerate() {
        let elem_patterns: Vec<Pattern> = tuple_patterns
            .iter()
            .filter_map(|tp| tp.get(i).cloned())
            .collect();

        let missing = find_missing_patterns(&elem_patterns, elem_ty, env);
        if !missing.is_empty() {
            // Build a representative missing pattern
            let mut parts: Vec<String> = (0..elem_types.len()).map(|_| "_".to_string()).collect();
            parts[i] = missing[0].clone();
            all_missing.push(format!("({})", parts.join(", ")));
        }
    }

    all_missing
}

/// Check List exhaustiveness: need empty and non-empty cases
fn check_list_exhaustiveness(
    patterns: &[Pattern],
    _elem_type: &Type,
    _env: &TypeEnv,
) -> Vec<String> {
    // If any pattern is a wildcard or bind, it's exhaustive
    if patterns.iter().any(|p| matches!(p, Pattern::Wildcard | Pattern::Bind(_))) {
        return vec![];
    }

    let mut has_empty = false;
    let mut has_nonempty = false;

    for pat in patterns {
        match pat {
            Pattern::List { items, rest } => {
                if items.is_empty() && rest.is_none() {
                    has_empty = true;
                } else if !items.is_empty() || rest.is_some() {
                    has_nonempty = true;
                }
            }
            Pattern::Wildcard | Pattern::Bind(_) => {
                has_empty = true;
                has_nonempty = true;
            }
            _ => {}
        }
    }

    let mut missing = vec![];
    if !has_empty {
        missing.push("[]".to_string());
    }
    if !has_nonempty {
        missing.push("[_, ...]".to_string());
    }
    missing
}

/// Check ADT exhaustiveness: need all constructors covered
fn check_adt_exhaustiveness(
    patterns: &[Pattern],
    type_name: &str,
    env: &TypeEnv,
) -> Vec<String> {
    // If any pattern is a wildcard or bind, it's exhaustive
    if patterns.iter().any(|p| matches!(p, Pattern::Wildcard | Pattern::Bind(_))) {
        return vec![];
    }

    // Get all constructors for this type
    let type_def = match env.type_defs.get(type_name) {
        Some(td) => td,
        None => return vec![], // Unknown type, assume exhaustive
    };

    let all_constructors: HashSet<&String> = type_def.variants.keys().collect();

    // Collect covered constructors
    let covered_constructors: HashSet<&String> = patterns
        .iter()
        .filter_map(|p| match p {
            Pattern::Constructor { name, .. } => Some(name),
            _ => None,
        })
        .collect();

    // Find missing constructors
    let missing: Vec<String> = all_constructors
        .difference(&covered_constructors)
        .map(|name| {
            let fields = type_def.variants.get(*name).unwrap();
            if fields.is_empty() {
                (*name).clone()
            } else {
                let field_patterns: Vec<String> = fields
                    .iter()
                    .map(|(fname, _)| format!("{}: _", fname))
                    .collect();
                format!("({} {{{}}})", name, field_patterns.join(", "))
            }
        })
        .collect();

    missing
}

/// Format a warning message for non-exhaustive patterns
pub fn format_warning(result: &ExhaustivenessResult) -> Option<String> {
    if result.is_exhaustive {
        None
    } else {
        let missing = result.missing_patterns.join(", ");
        Some(format!("non-exhaustive patterns: missing {}", missing))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bool_exhaustive() {
        let patterns = vec![Pattern::Bool(true), Pattern::Bool(false)];
        let result = check_exhaustiveness(&patterns, &Type::Bool, &TypeEnv::default());
        assert!(result.is_exhaustive);
    }

    #[test]
    fn test_bool_missing_false() {
        let patterns = vec![Pattern::Bool(true)];
        let result = check_exhaustiveness(&patterns, &Type::Bool, &TypeEnv::default());
        assert!(!result.is_exhaustive);
        assert!(result.missing_patterns.contains(&"false".to_string()));
    }

    #[test]
    fn test_bool_with_wildcard() {
        let patterns = vec![Pattern::Bool(true), Pattern::Wildcard];
        let result = check_exhaustiveness(&patterns, &Type::Bool, &TypeEnv::default());
        assert!(result.is_exhaustive);
    }

    #[test]
    fn test_number_needs_wildcard() {
        let patterns = vec![Pattern::Number(1.0), Pattern::Number(2.0)];
        let result = check_exhaustiveness(&patterns, &Type::Number, &TypeEnv::default());
        assert!(!result.is_exhaustive);
    }

    #[test]
    fn test_number_with_bind() {
        let patterns = vec![Pattern::Bind("n".to_string())];
        let result = check_exhaustiveness(&patterns, &Type::Number, &TypeEnv::default());
        assert!(result.is_exhaustive);
    }

    #[test]
    fn test_list_exhaustive() {
        let patterns = vec![
            Pattern::List { items: vec![], rest: None },
            Pattern::List { items: vec![Pattern::Bind("h".to_string())], rest: Some("t".to_string()) },
        ];
        let result = check_exhaustiveness(&patterns, &Type::List(Box::new(Type::Number)), &TypeEnv::default());
        assert!(result.is_exhaustive);
    }

    #[test]
    fn test_list_missing_empty() {
        let patterns = vec![
            Pattern::List { items: vec![Pattern::Bind("h".to_string())], rest: Some("t".to_string()) },
        ];
        let result = check_exhaustiveness(&patterns, &Type::List(Box::new(Type::Number)), &TypeEnv::default());
        assert!(!result.is_exhaustive);
        assert!(result.missing_patterns.contains(&"[]".to_string()));
    }
}
