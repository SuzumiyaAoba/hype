use insta::assert_snapshot;

#[test]
fn algebraic_data_types() {
    let cases = [
        // Basic ADT definition
        "type Bool = True | False; True",
        // ADT with field
        "type Box = Value { x: Number }; Value { x: 42 }",
        // Parameterized ADT
        "type Option<A> = None | Some { value: A }; None",
        "type Option<A> = None | Some { value: A }; Some { value: 42 }",
        // Pattern match on ADT
        "type Bool = True | False; match(True) { case True => 1; case False => 0 }",
        // Pattern match with field binding
        "type Option<A> = None | Some { value: A }; let x = Some { value: 10 }; match(x) { case None => 0; case Some { value: v } => v }",
        // Multiple fields
        "type Pair = MkPair { fst: Number, snd: Number }; let p = MkPair { fst: 1, snd: 2 }; match(p) { case MkPair { fst: x, snd: y } => x + y }",
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
