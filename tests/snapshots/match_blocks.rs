use insta::assert_snapshot;

#[test]
fn match_and_blocks() {
    let cases = [
        "(do (let [x 1] (+ x 2)))",
        "(match true [true 1] [_ 0])",
        "(match false [true (do (let [x 2] x))] [_ (do (let [y 3] (+ y 1)))])",
        "(match (and (< 1 2) true) [true 1] [_ 0])",
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
