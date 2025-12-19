use insta::assert_snapshot;

#[test]
fn match_guards() {
    let cases = [
        // Simple guard on number pattern
        "(match 5 [n :when (> n 3) \"big\"] [_ \"small\"])",
        // Guard with bound variable
        "(match 10 [x :when (== x 10) \"ten\"] [x :when (> x 5) \"big\"] [_ \"other\"])",
        // Guard on boolean pattern (redundant but valid)
        "(match true [b :when b \"truthy\"] [_ \"falsy\"])",
    ];

    let mut out = String::new();
    for (i, case) in cases.iter().enumerate() {
        if i > 0 {
            out.push_str("===\n");
        }
        let js = hype::transpile(case).unwrap();
        out.push_str(case);
        out.push_str("\n---\n");
        out.push_str(&js);
        out.push('\n');
    }

    assert_snapshot!(out);
}

#[test]
fn match_guards_with_recursion() {
    // Classic factorial with guards for termination
    let code = r#"
(defn factorial [n: Number] -> Number
  (match n
    [0 1]
    [n :when (> n 0) (* n (factorial (- n 1)))]))

(factorial 5)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn match_guards_with_list() {
    // List processing with guards
    let code = r#"
(defn sum_big [xs: (List Number)] -> Number
  (match xs
    [[] 0]
    [[h ...t] :when (> h 5) (+ h (sum_big t))]
    [[_ ...t] (sum_big t)]))

(sum_big [1 2 10 3 20])
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}
