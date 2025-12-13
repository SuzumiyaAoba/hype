use insta::assert_snapshot;

fn snap_err(src: &str) -> String {
    let e = hype::transpile(src).unwrap_err();
    hype::format_error(&e)
}

#[test]
fn error_outputs() {
    let cases = [
        "x",
        "fn f(a: Number): Number = a; f(true)",
        "1 + true",
    ];

    let mut out = String::new();
    for (i, case) in cases.iter().enumerate() {
        if i > 0 {
            out.push_str("===\n");
        }
        let err = snap_err(case);
        out.push_str(case);
        out.push_str("\n---\n");
        out.push_str(&err);
        out.push('\n');
    }

    assert_snapshot!(out);
}
