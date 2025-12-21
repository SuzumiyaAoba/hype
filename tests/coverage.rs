use hype;

#[test]
fn test_coverage_initialization() {
    // Set coverage environment variable
    unsafe {
        std::env::set_var("HYPE_COVERAGE", "1");
    }

    let code = r#"
(defn add [x y]
  (+ x y))

(add 1 2)
"#;
    let result = hype::transpile(code);
    assert!(result.is_ok(), "Failed to transpile: {:?}", result.err());

    let js = result.unwrap();

    // Check that coverage initialization code is present
    assert!(js.contains("__hype_coverage"), "Missing coverage initialization");
    assert!(js.contains("total:"), "Missing total count initialization");
    assert!(js.contains("executed:"), "Missing executed set initialization");
    assert!(js.contains("__hype_coverage.total ="), "Missing total assignment");

    // Clean up
    unsafe {
        std::env::remove_var("HYPE_COVERAGE");
    }
}

#[test]
fn test_coverage_instrumentation() {
    unsafe {
        std::env::set_var("HYPE_COVERAGE", "1");
    }

    let code = r#"
(defn greet [name]
  (+ "Hello, " name))

(let [x 42]
  (greet "World"))
"#;
    let result = hype::transpile(code);
    assert!(result.is_ok(), "Failed to transpile: {:?}", result.err());

    let js = result.unwrap();

    // Check that coverage tracking calls are inserted
    assert!(js.contains("__hype_coverage.executed.add"), "Missing coverage tracking");

    // Should have tracking for: fn definition and let expression
    let tracking_count = js.matches("__hype_coverage.executed.add").count();
    assert_eq!(tracking_count, 2, "Expected 2 tracking calls, got {}", tracking_count);

    unsafe {
        std::env::remove_var("HYPE_COVERAGE");
    }
}

#[test]
fn test_coverage_import() {
    unsafe {
        std::env::set_var("HYPE_COVERAGE", "1");
    }

    let code = r#"
(import "std/coverage.ffi.hp")

(defn add [x y]
  (+ x y))

(add 1 2)
"#;
    let result = hype::transpile(code);
    assert!(result.is_ok(), "Failed to transpile: {:?}", result.err());

    let js = result.unwrap();

    // Check that coverage module functions are imported
    assert!(js.contains("const get_coverage_percentage"), "Missing get_coverage_percentage");
    assert!(js.contains("const print_coverage_report"), "Missing print_coverage_report");

    unsafe {
        std::env::remove_var("HYPE_COVERAGE");
    }
}

#[test]
fn test_no_coverage_when_disabled() {
    // Make sure HYPE_COVERAGE is not set - remove it explicitly
    // This is important because other tests may have set it
    unsafe {
        std::env::remove_var("HYPE_COVERAGE");
    }

    // Give the environment variable change time to propagate
    std::thread::sleep(std::time::Duration::from_millis(10));

    let code = r#"
(defn add [x y]
  (+ x y))

(add 1 2)
"#;
    let result = hype::transpile(code);
    assert!(result.is_ok(), "Failed to transpile: {:?}", result.err());

    let js = result.unwrap();

    // Check that coverage code is NOT present
    assert!(!js.contains("__hype_coverage"), "Coverage code should not be present when disabled");
}
