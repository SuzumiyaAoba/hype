use hype::{format_error, transpile};

fn err(input: &str) -> String {
    let e = transpile(input).unwrap_err();
    format_error(&e)
}

#[test]
fn bad_bool_in_number_binop() {
    let out = err("1 + true");
    assert!(out.contains("type mismatch for '+'"));
}

#[test]
fn bad_arg_type() {
    let out = err("fn f(a: Number): Number = a; f(true)");
    assert!(out.contains("type mismatch in argument"));
}

#[test]
fn bad_match_arm_type() {
    let out = err("match(true){case true => 1; case false => \"no\"}");
    assert!(out.contains("match arms must have the same type"));
}

#[test]
fn bad_block_last_expr() {
    let out = err("{ let x: Number = 1; }");
    assert!(out.contains("block must end with expression"));
}
