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
        ("lambda_let", "let id = \\x -> x; id(3)"),
        (
            "lambda_closure",
            "fn wrap(x) = { let add = \\y -> x + y; add }; wrap(1)",
        ),
        (
            "mutual_in_block",
            "{ fn is_even(n) = match(n){case 0 => true; case _ => is_odd(n - 1)}; fn is_odd(n) = match(n){case 0 => false; case _ => is_even(n - 1)}; is_even(6) }",
        ),
        (
            "forward_call_to_mutual",
            "start(); fn start() = is_even(3); fn is_even(n) = match(n){case 0 => true; case _ => is_odd(n - 1)}; fn is_odd(n) = match(n){case 0 => false; case _ => is_even(n - 1)}",
        ),
        (
            "let_rec_factorial",
            "let rec fact = \\n -> match(n){case 0 => 1; case _ => n * fact(n - 1)}; fact(5)",
        ),
        (
            "let_rec_in_block",
            "{ let rec sum = \\n -> match(n){case 0 => 0; case _ => n + sum(n - 1)}; sum(3) }",
        ),
        ("tuple_literal", "(1, true, \"s\")"),
        (
            "tuple_pattern_match",
            "match((1, true)){case (1, true) => 0; case _ => 1}",
        ),
        ("list_literal", "[1, 2, 3]"),
        (
            "list_cons_match",
            "match([1, 2]){case [h, ...t] => h; case _ => 0}",
        ),
        (
            "list_rest_ignored",
            "match([1, 2]){case [h, ...] => h; case _ => 0}",
        ),
        (
            "fix_factorial",
            "let fact = fix(\\rec -> \\n -> match(n){case 0 => 1; case _ => n * rec(n - 1)}); fact(4)",
        ),
        (
            "external_simple",
            "external log: String -> Unit = \"console.log\"; log(\"hello\")",
        ),
        (
            "external_with_return",
            "external parse_int: String -> Number = \"parseInt\"; parse_int(\"42\") + 1",
        ),
        (
            "external_multi_arg",
            "external pow: (Number, Number) -> Number = \"Math.pow\"; pow(2, 10)",
        ),
        (
            "curried_function",
            "let add = \\x -> \\y -> x + y; add(1)(2)",
        ),
        (
            "chained_call_make_adder",
            "fn make_adder(x: Number) = \\y -> x + y; make_adder(5)(10)",
        ),
        (
            "nested_closure_capture",
            "fn outer(x: Number) = { fn inner(y: Number) = \\z -> x + y + z; inner }; outer(1)(2)(3)",
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
