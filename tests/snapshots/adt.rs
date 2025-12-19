use insta::assert_snapshot;

#[test]
fn algebraic_data_types() {
    let cases = [
        // Basic ADT definition
        "(deftype Bool True False) True",
        // ADT with field
        "(deftype Box (Value {:x Number})) (Value {:x 42})",
        // Parameterized ADT
        "(deftype Option [A] None (Some {:value A})) None",
        "(deftype Option [A] None (Some {:value A})) (Some {:value 42})",
        // Pattern match on ADT
        "(deftype Bool True False) (match True [True 1] [False 0])",
        // Pattern match with field binding
        "(deftype Option [A] None (Some {:value A})) (let [x (Some {:value 10})] (match x [None 0] [(Some {:value v}) v]))",
        // Multiple fields
        "(deftype Pair (MkPair {:fst Number :snd Number})) (let [p (MkPair {:fst 1 :snd 2})] (match p [(MkPair {:fst x :snd y}) (+ x y)]))",
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
