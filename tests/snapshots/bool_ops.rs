use insta::assert_snapshot;

#[test]
fn bool_and_comparison() {
    let cases = [
        "true",
        "false",
        "true && false",
        "true || false && true",
        "1 < 2",
        "1 <= 2 == true",
        "1 + 2 == 3",
        "fn eq(a: Number, b: Number): Bool = a == b;\neq(1, 1)",
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
