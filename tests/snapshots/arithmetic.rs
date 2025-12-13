use insta::assert_snapshot;

#[test]
fn arithmetic_and_lets() {
    let cases = [
        "1 + 2 * 3",
        "(1 + 2) * (3 - 4 / 2)",
        "-1 + 2 * 3",
        "let x = 1 + 2;\nlet y = x * 3;\ny - x",
        "fn add(a, b) = a + b;\nlet x = add(2, 3);\nadd(x, 4)",
        "let hello = \"hi\";\nhello + \"!\"",
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
