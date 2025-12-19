use insta::assert_snapshot;

#[test]
fn exhaustive_patterns() {
    // These should compile without warnings
    let cases = [
        // Bool - exhaustive with both values
        "(match true [true 1] [false 0])",
        // Bool - exhaustive with wildcard
        "(match true [true 1] [_ 0])",
        // Number - exhaustive with bind
        "(match 42 [n n])",
        // Number - exhaustive with wildcard
        "(match 42 [_ 0])",
        // List - exhaustive with empty and non-empty
        "(match [1 2 3] [[] 0] [[h ...t] h])",
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
fn adt_exhaustive_patterns() {
    // ADT with all constructors covered
    let code = r#"
(deftype Option [A]
  None
  (Some {:value A}))

(defn unwrap_or [opt: (Option Number) default: Number] -> Number
  (match opt
    [None default]
    [(Some {:value v}) v]))

(unwrap_or (Some {:value 42}) 0)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}
