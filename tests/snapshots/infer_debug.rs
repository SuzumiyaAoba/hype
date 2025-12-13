use insta::assert_snapshot;

#[test]
fn inference_debug_output() {
    let mut dbg = hype::DebugInfo::default();
    let _ = hype::transpile_with_debug("let x = 1 + 2; x * 2", Some(&mut dbg)).unwrap();
    let text = hype::render_debug_text(&dbg);
    assert_snapshot!(text);
}
