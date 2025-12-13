use hype::{format_error, transpile};

fn err(input: &str) -> String {
    let e = transpile(input).unwrap_err();
    format_error(&e)
}

#[test]
fn bad_bool_in_number_binop() {
    let out = err("1 + true");
    assert!(out.contains("type mismatch"));
}

#[test]
fn bad_arg_type() {
    let out = err("fn f(a: Number): Number = a; f(true)");
    assert!(out.contains("type mismatch"));
}

#[test]
fn bad_match_arm_type() {
    let out = err("match(true){case true => 1; case false => \"no\"}");
    assert!(out.contains("type mismatch") || out.contains("match arms"));
}

#[test]
fn bad_block_last_expr() {
    let out = err("{ let x: Number = 1; }");
    assert!(out.contains("block must end with expression"));
}

#[test]
fn occurs_check_rejected() {
    let out = err("fn self(x) = x(x); self(self)");
    assert!(out.contains("occurs check"));
}

#[test]
fn arity_mismatch() {
    let out = err("fn add(a, b) = a + b; add(1)");
    assert!(out.contains("arity mismatch"));
}
