#[test]
fn polymorphic_fn_usage() {
    let src = "fn id(a) = a; id(1); id(true)";
    let js = hype::transpile(src).unwrap();
    assert_eq!(js, "function id(a) { return a; }\nid(1);\nid(true)");
}
