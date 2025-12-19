use insta::assert_snapshot;

fn js(src: &str) -> String {
    hype::transpile(src).unwrap()
}

#[test]
fn transformations() {
    let cases = [
        ("polymorphic_fn", "(defn id [a] a) (id 1) (id true)"),
        (
            "higher_order_apply",
            "(defn apply [f x] (f x)) (defn inc [n] (+ n 1)) (apply inc 41)",
        ),
        ("polymorphic_predicate", "(defn eq [x y] (== x y)) (eq 1 1) (eq \"a\" \"b\")"),
        (
            "recursive_fact",
            "(defn fact [n] (match n [0 1] [_ (* n (fact (- n 1)))])) (fact 5)",
        ),
        (
            "mutual_even_odd",
            "(defn is_even [n] (match n [0 true] [_ (is_odd (- n 1))])) (defn is_odd [n] (match n [0 false] [_ (is_even (- n 1))])) (is_even 4) (is_odd 5)",
        ),
        ("lambda_let", "(let [id (fn [x] x)] (id 3))"),
        (
            "lambda_closure",
            "(defn wrap [x] (do (let [add (fn [y] (+ x y))] add))) (wrap 1)",
        ),
        (
            "mutual_in_block",
            "(do (defn is_even [n] (match n [0 true] [_ (is_odd (- n 1))])) (defn is_odd [n] (match n [0 false] [_ (is_even (- n 1))])) (is_even 6))",
        ),
        (
            "forward_call_to_mutual",
            "(start) (defn start [] (is_even 3)) (defn is_even [n] (match n [0 true] [_ (is_odd (- n 1))])) (defn is_odd [n] (match n [0 false] [_ (is_even (- n 1))]))",
        ),
        (
            "let_rec_factorial",
            "(let-rec [fact (fn [n] (match n [0 1] [_ (* n (fact (- n 1)))]))] (fact 5))",
        ),
        (
            "let_rec_in_block",
            "(do (let-rec [sum (fn [n] (match n [0 0] [_ (+ n (sum (- n 1)))]))] (sum 3)))",
        ),
        ("tuple_literal", "[1, true, \"s\"]"),
        (
            "tuple_pattern_match",
            "(match [1, true] [[1, true] 0] [_ 1])",
        ),
        ("list_literal", "[1 2 3]"),
        (
            "list_cons_match",
            "(match [1 2] [[h ...t] h] [_ 0])",
        ),
        (
            "list_rest_ignored",
            "(match [1 2] [[h ...] h] [_ 0])",
        ),
        (
            "fix_factorial",
            "(let [fact (fix (fn [rec] (fn [n] (match n [0 1] [_ (* n (rec (- n 1)))]))))] (fact 4))",
        ),
        (
            "external_simple",
            "(external log (-> String Unit) \"console.log\") (log \"hello\")",
        ),
        (
            "external_with_return",
            "(external parse_int (-> String Number) \"parseInt\") (+ (parse_int \"42\") 1)",
        ),
        (
            "external_multi_arg",
            "(external pow (-> Number Number Number) \"Math.pow\") (pow 2 10)",
        ),
        (
            "curried_function",
            "(let [add (fn [x] (fn [y] (+ x y)))] ((add 1) 2))",
        ),
        (
            "chained_call_make_adder",
            "(defn make_adder [x: Number] (fn [y] (+ x y))) ((make_adder 5) 10)",
        ),
        (
            "nested_closure_capture",
            "(defn outer [x: Number] (do (defn inner [y: Number] (fn [z] (+ (+ x y) z))) inner)) (((outer 1) 2) 3)",
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
