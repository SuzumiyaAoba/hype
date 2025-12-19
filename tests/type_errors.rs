use hype::{format_error, transpile};

fn err(input: &str) -> String {
    let e = transpile(input).unwrap_err();
    format_error(&e)
}

#[test]
fn bad_bool_in_number_binop() {
    let out = err("(+ 1 true)");
    assert!(out.contains("type mismatch"));
}

#[test]
fn bad_arg_type() {
    // (defn f [a: Number] -> Number a) (f true)
    let out = err("(defn f [a: Number] -> Number a) (f true)");
    assert!(out.contains("type mismatch"));
}

#[test]
fn bad_match_arm_type() {
    // (match true [true 1] [false "no"])
    let out = err("(match true [true 1] [false \"no\"])");
    assert!(out.contains("type mismatch") || out.contains("match arms"));
}

#[test]
fn occurs_check_rejected() {
    // (defn self [x] (x x)) (self self)
    let out = err("(defn self [x] (x x)) (self self)");
    assert!(out.contains("occurs check"));
}

#[test]
fn arity_mismatch() {
    // (defn add [a b] (+ a b)) (add 1)
    let out = err("(defn add [a b] (+ a b)) (add 1)");
    assert!(out.contains("arity mismatch"));
}
