#[test]
fn polymorphic_fn_usage() {
    let src = "fn id(a) = a; id(1); id(true)";
    let js = hype::transpile(src).unwrap();
    assert_eq!(js, "function id(a) { return a; }\nid(1);\nid(true)");
}

#[test]
fn higher_order_function_inference() {
    let src = "fn apply(f, x) = f(x); fn inc(n) = n + 1; apply(inc, 41)";
    let js = hype::transpile(src).unwrap();
    assert_eq!(
        js,
        "function apply(f, x) { return f(x); }\nfunction inc(n) { return n + 1; }\napply(inc, 41)"
    );
}

#[test]
fn polymorphic_predicate_inference() {
    let src = "fn eq(x, y) = x == y; eq(1, 1); eq(\"a\", \"b\")";
    let js = hype::transpile(src).unwrap();
    assert_eq!(
        js,
        "function eq(x, y) { return x == y; }\neq(1, 1);\neq(\"a\", \"b\")"
    );
}
