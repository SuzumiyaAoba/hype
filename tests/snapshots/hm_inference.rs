use insta::assert_snapshot;

fn js(src: &str) -> String {
    hype::transpile(src).unwrap()
}

#[test]
fn transformations() {
    let cases = [
        ("polymorphic_fn", "fn id(a) = a; id(1); id(true)"),
        (
            "higher_order_apply",
            "fn apply(f, x) = f(x); fn inc(n) = n + 1; apply(inc, 41)",
        ),
        ("polymorphic_predicate", "fn eq(x, y) = x == y; eq(1, 1); eq(\"a\", \"b\")"),
        (
            "recursive_fact",
            "fn fact(n) = match(n){case 0 => 1; case _ => n * fact(n - 1)}; fact(5)",
        ),
        (
            "mutual_even_odd",
            "fn is_even(n) = match(n){case 0 => true; case _ => is_odd(n - 1)};\nfn is_odd(n) = match(n){case 0 => false; case _ => is_even(n - 1)};\nis_even(4); is_odd(5)",
        ),
        ("lambda_let", "let id = fn(x) = x; id(3)"),
        (
            "lambda_closure",
            "fn wrap(x) = { let add = fn(y) = x + y; add }; wrap(1)",
        ),
    ];

    let mut out = String::new();
    for (i, (name, src)) in cases.iter().enumerate() {
        if i > 0 {
            out.push_str("===\n");
        }
        out.push_str(name);
        out.push('\n');
        out.push_str(src);
        out.push_str("\n---\n");
        out.push_str(&js(src));
        out.push('\n');
    }

    assert_snapshot!(out);
}
