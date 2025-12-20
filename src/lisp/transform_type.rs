use crate::ast::Type;
use super::parser::Sexp;

pub fn parse_type(sexp: &Sexp) -> Result<Type, String> {
    match sexp {
        // Simple types: Number, String, Bool, or type variable/ADT
        Sexp::Symbol(name) => match name.as_str() {
            "Number" => Ok(Type::Number),
            "String" => Ok(Type::String),
            "Bool" => Ok(Type::Bool),
            "Unit" => Ok(Type::Unit),
            _ => {
                // Treat uppercase-starting names as type variables or user-defined types
                if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                    // Could be a type variable like A, B, T, etc., or a user-defined ADT
                    Ok(Type::Adt {
                        name: name.clone(),
                        args: Vec::new(),
                    })
                } else {
                    Err(format!("Unknown type: {}", name))
                }
            }
        },

        // Arrow-based function type: (-> A B C) means A -> B -> C
        Sexp::List(items) if !items.is_empty() && matches!(&items[0], Sexp::Arrow) => {
            if items.len() < 3 {
                return Err("Arrow function type requires at least input and output types".to_string());
            }
            // (-> A B) means A -> B
            // (-> A B C) means (A, B) -> C
            let last_idx = items.len() - 1;
            let ret = parse_type(&items[last_idx])?;
            let params: Result<Vec<_>, _> = items[1..last_idx]
                .iter()
                .map(parse_type)
                .collect();
            Ok(Type::Fun(params?, Box::new(ret)))
        }

        // Parametric types: (List Number), (Option String)
        Sexp::List(items) if !items.is_empty() => {
            match &items[0] {
                Sexp::Symbol(name) if name == "List" => {
                    if items.len() != 2 {
                        return Err("List type requires exactly one type argument".to_string());
                    }
                    let elem_type = parse_type(&items[1])?;
                    Ok(Type::List(Box::new(elem_type)))
                }
                Sexp::Symbol(name) if name == "Tuple" => {
                    let elem_types: Result<Vec<_>, _> = items[1..]
                        .iter()
                        .map(parse_type)
                        .collect();
                    Ok(Type::Tuple(elem_types?))
                }
                Sexp::Symbol(name) if name == "fn" => {
                    // Function type: (fn [Number String] -> Bool)
                    if items.len() < 3 {
                        return Err("Function type requires parameter types and return type".to_string());
                    }

                    // Parse parameter types
                    let params = match &items[1] {
                        Sexp::Vector(params) => {
                            params.iter().map(parse_type).collect::<Result<Vec<_>, _>>()?
                        }
                        _ => return Err("Function type parameters must be in a vector".to_string()),
                    };

                    // Check for ->
                    if !matches!(&items[2], Sexp::Symbol(s) if s == "->") {
                        return Err("Function type must have -> before return type".to_string());
                    }

                    // Parse return type
                    if items.len() != 4 {
                        return Err("Function type requires exactly one return type".to_string());
                    }
                    let ret = parse_type(&items[3])?;

                    Ok(Type::Fun(params, Box::new(ret)))
                }
                Sexp::Symbol(name) => {
                    // ADT type: (Option String)
                    let args: Result<Vec<_>, _> = items[1..]
                        .iter()
                        .map(parse_type)
                        .collect();
                    Ok(Type::Adt {
                        name: name.clone(),
                        args: args?,
                    })
                }
                _ => Err(format!("Invalid type expression: {:?}", sexp)),
            }
        }

        // Record type: {:x Number :y String}
        Sexp::Map(pairs) => {
            let mut fields = Vec::new();
            for (key, value) in pairs {
                let field_name = match key {
                    Sexp::Symbol(n) => n.clone(),
                    _ => return Err("Record field name must be a symbol".to_string()),
                };
                let field_type = parse_type(value)?;
                fields.push((field_name, field_type));
            }
            Ok(Type::Record(fields))
        }

        _ => Err(format!("Invalid type: {:?}", sexp)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_types() {
        assert_eq!(parse_type(&Sexp::Symbol("Number".to_string())).unwrap(), Type::Number);
        assert_eq!(parse_type(&Sexp::Symbol("String".to_string())).unwrap(), Type::String);
        assert_eq!(parse_type(&Sexp::Symbol("Bool".to_string())).unwrap(), Type::Bool);
    }

    #[test]
    fn test_parse_list_type() {
        let sexp = Sexp::List(vec![
            Sexp::Symbol("List".to_string()),
            Sexp::Symbol("Number".to_string()),
        ]);
        let ty = parse_type(&sexp).unwrap();
        assert!(matches!(ty, Type::List(_)));
    }

    #[test]
    fn test_parse_tuple_type() {
        let sexp = Sexp::List(vec![
            Sexp::Symbol("Tuple".to_string()),
            Sexp::Symbol("Number".to_string()),
            Sexp::Symbol("String".to_string()),
        ]);
        let ty = parse_type(&sexp).unwrap();
        assert!(matches!(ty, Type::Tuple(_)));
    }

    #[test]
    fn test_parse_function_type() {
        let sexp = Sexp::List(vec![
            Sexp::Symbol("fn".to_string()),
            Sexp::Vector(vec![
                Sexp::Symbol("Number".to_string()),
                Sexp::Symbol("String".to_string()),
            ]),
            Sexp::Symbol("->".to_string()),
            Sexp::Symbol("Bool".to_string()),
        ]);
        let ty = parse_type(&sexp).unwrap();
        assert!(matches!(ty, Type::Fun(_, _)));
    }
}
