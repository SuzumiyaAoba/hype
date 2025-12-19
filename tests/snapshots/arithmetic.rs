use insta::assert_snapshot;

#[test]
fn arithmetic_and_lets() {
    let cases = [
        "(+ 1 (* 2 3))",
        "(* (+ 1 2) (- 3 (/ 4 2)))",
        "(+ (- 1) (* 2 3))",
        "(let [x (+ 1 2)] (let [y (* x 3)] (- y x)))",
        "(defn add [a: Number b: Number] -> Number (+ a b)) (let [x (add 2 3)] (add x 4))",
        "(let [hello \"hi\"] (+ hello \"!\"))",
        "(let [x (+ 1 2)] x)",
        "(defn add1 [a: Number] (+ a 1)) (add1 2)",
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
