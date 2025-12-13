use insta::assert_snapshot;

#[test]
fn match_and_blocks() {
    let cases = [
        "{ let x: Number = 1; x + 2 }",
        "match(true){case true => 1; case _ => 0}",
        "match(false){case true => { let x: Number = 2; x }; case _ => { let y: Number = 3; y + 1 }}",
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
