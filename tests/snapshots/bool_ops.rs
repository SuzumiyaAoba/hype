use insta::assert_snapshot;

#[test]
fn bool_and_comparison() {
    let cases = [
        "true",
        "false",
        "(and true false)",
        "(or true (and false true))",
        "(or (and false true) true)",
        "(or (and true true) false)",
        "(< 1 2)",
        "(== (<= 1 2) true)",
        "(== (+ 1 2) 3)",
        "(defn eq [a: Number b: Number] -> Bool (== a b)) (eq 1 1)",
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
