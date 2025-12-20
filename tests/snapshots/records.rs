use insta::assert_snapshot;

#[test]
fn record_literals() {
    let cases = [
        // Simple record
        "{:x 1 :y 2}",
        // Record with string field
        "{:name \"Alice\" :age 30}",
        // Nested record
        "{:point {:x 1 :y 2} :label \"origin\"}",
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
fn field_access() {
    let code = r#"
(let [point {:x 10 :y 20}]
  (+ (.x point) (.y point)))
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn record_pattern_matching() {
    let code = r#"
(let [person {:name "Bob" :age 25}]
  (match person
    [{:name n :age a} n]))
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn record_with_type_annotation() {
    let code = r#"
(defn get_x [p: {:x Number :y Number}] -> Number
  (.x p))

(get_x {:x 42 :y 0})
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn nested_field_access() {
    let code = r#"
(let [data {:inner {:value 100}}]
  (.value (.inner data)))
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}
