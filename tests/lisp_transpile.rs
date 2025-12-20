use hype::{parse_lisp, render_program};

fn transpile_lisp(input: &str) -> Result<String, String> {
    let stmts = parse_lisp(input).map_err(|e| e.message)?;
    Ok(render_program(&stmts))
}

#[test]
fn test_transpile_number() {
    let result = transpile_lisp("42").unwrap();
    assert!(result.contains("42"));
}

#[test]
fn test_transpile_simple_add() {
    let result = transpile_lisp("(+ 1 2)").unwrap();
    assert!(result.contains("1 + 2") || result.contains("(1 + 2)"));
}

#[test]
fn test_transpile_nested_arithmetic() {
    let result = transpile_lisp("(+ (* 2 3) 4)").unwrap();
    // Should contain multiplication and addition
    assert!(result.contains("*"));
    assert!(result.contains("+"));
    assert!(result.contains("2"));
    assert!(result.contains("3"));
    assert!(result.contains("4"));
}

#[test]
fn test_transpile_let_binding() {
    let result = transpile_lisp("(let [x 10] (+ x 5))").unwrap();
    // Should declare x and use it
    assert!(result.contains("x"));
    assert!(result.contains("10"));
    assert!(result.contains("5"));
}

#[test]
fn test_transpile_multiple_bindings() {
    let result = transpile_lisp("(let [x 10 y 20] (+ x y))").unwrap();
    assert!(result.contains("x"));
    assert!(result.contains("y"));
    assert!(result.contains("10"));
    assert!(result.contains("20"));
}

#[test]
fn test_transpile_defn() {
    let result = transpile_lisp("(defn add [x y] (+ x y))").unwrap();
    // Should define a function named add
    assert!(result.contains("add"));
    assert!(result.contains("function") || result.contains("=>"));
    assert!(result.contains("x"));
    assert!(result.contains("y"));
}

#[test]
fn test_transpile_function_call() {
    let result = transpile_lisp("(defn double [x] (* x 2)) (double 5)").unwrap();
    assert!(result.contains("double"));
    assert!(result.contains("5"));
}

#[test]
fn test_transpile_do_block() {
    let result = transpile_lisp(
        r#"
        (do
          (let [x 10] x)
          (let [y 20] y)
          (+ x y))
    "#,
    )
    .unwrap();
    assert!(result.contains("x"));
    assert!(result.contains("y"));
}

#[test]
fn test_transpile_comparison() {
    let result = transpile_lisp("(< 1 2)").unwrap();
    assert!(result.contains("<"));
    assert!(result.contains("1"));
    assert!(result.contains("2"));
}

#[test]
fn test_transpile_lambda() {
    let result = transpile_lisp("(fn [x] (* x 2))").unwrap();
    assert!(result.contains("function") || result.contains("=>"));
    assert!(result.contains("x"));
    assert!(result.contains("2"));
}

#[test]
fn test_transpile_list_literal() {
    let result = transpile_lisp("[1 2 3]").unwrap();
    assert!(result.contains("1"));
    assert!(result.contains("2"));
    assert!(result.contains("3"));
}

#[test]
fn test_transpile_string() {
    let result = transpile_lisp(r#""hello world""#).unwrap();
    assert!(result.contains("hello world"));
}

#[test]
fn test_transpile_boolean() {
    let result = transpile_lisp("true").unwrap();
    assert!(result.contains("true"));

    let result = transpile_lisp("false").unwrap();
    assert!(result.contains("false"));
}

#[test]
fn test_transpile_logical_operators() {
    let result = transpile_lisp("(and true false)").unwrap();
    assert!(result.contains("&&"));

    let result = transpile_lisp("(or true false)").unwrap();
    assert!(result.contains("||"));
}
