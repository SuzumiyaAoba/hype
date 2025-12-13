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

#[test]
fn recursive_factorial() {
    let src = "fn fact(n) = match(n){case 0 => 1; case _ => n * fact(n - 1)}; fact(5)";
    let js = hype::transpile(src).unwrap();
    assert_eq!(
        js,
        "function fact(n) { return (() => { const __match = n; if (__match === 0) { return 1; } else if (true) { return n * fact(n - 1); } return undefined; })(); }\nfact(5)"
    );
}

#[test]
fn mutual_recursion_even_odd() {
    let src = "\
fn is_even(n) = match(n){case 0 => true; case _ => is_odd(n - 1)};
fn is_odd(n) = match(n){case 0 => false; case _ => is_even(n - 1)};
is_even(4); is_odd(5)";
    let js = hype::transpile(src).unwrap();
    assert_eq!(
        js,
        "function is_even(n) { return (() => { const __match = n; if (__match === 0) { return true; } else if (true) { return is_odd(n - 1); } return undefined; })(); }\nfunction is_odd(n) { return (() => { const __match = n; if (__match === 0) { return false; } else if (true) { return is_even(n - 1); } return undefined; })(); }\nis_even(4);\nis_odd(5)"
    );
}
