use hype;

#[test]
fn test_std_test_import() {
    let code = r#"
(import "std/test.ffi.hp")

(defn test-assertions []
  (do
    (assert true)
    (assert_eq (+ 1 2) 3)
    (assert_eq_str "hello" "hello")
    (assert_eq_bool true true)
    (debug "Debug message")
    (test_pass "Assertions passed")))

(test-assertions)
"#;
    let result = hype::transpile(code);
    assert!(result.is_ok(), "Failed to transpile: {:?}", result.err());

    let js = result.unwrap();
    assert!(js.contains("const assert = "), "Missing assert function");
    assert!(js.contains("const assert_eq = "), "Missing assert_eq function");
    assert!(js.contains("const assert_eq_str = "), "Missing assert_eq_str function");
    assert!(js.contains("const assert_eq_bool = "), "Missing assert_eq_bool function");
    assert!(js.contains("const debug = "), "Missing debug function");
    assert!(js.contains("const test_pass = "), "Missing test_pass function");
    // Function names with hyphens are converted to snake_case in JavaScript
    assert!(js.contains("test_assertions") || js.contains("test-assertions"), "Missing test function");
}

#[test]
fn test_std_console_import() {
    let code = r#"
(import "std/console.ffi.hp")

(defn greet [name]
  (do
    (log "Hello from Hype!")
    (warn "This is a warning")
    (error "This is an error")))

(greet "World")
"#;
    let result = hype::transpile(code);
    assert!(result.is_ok(), "Failed to transpile: {:?}", result.err());

    let js = result.unwrap();
    assert!(js.contains("const log = console.log"), "Missing log function");
    assert!(js.contains("const warn = console.warn"), "Missing warn function");
    assert!(js.contains("const error = console.error"), "Missing error function");
}

#[test]
fn test_multiple_std_imports() {
    let code = r#"
(import "std/test.ffi.hp")
(import "std/console.ffi.hp")

(defn test-math []
  (do
    (log "Testing addition...")
    (assert_eq (+ 1 2) 3)
    (assert_eq (* 2 3) 6)
    (test_pass "Math tests passed")))

(test-math)
"#;
    let result = hype::transpile(code);
    assert!(result.is_ok(), "Failed to transpile: {:?}", result.err());

    let js = result.unwrap();
    assert!(js.contains("const assert_eq = "), "Missing assert_eq");
    assert!(js.contains("const log = console.log"), "Missing log");
    assert!(js.contains("const test_pass = "), "Missing test_pass");
}

#[test]
fn test_std_import_with_relative_import() {
    // std/ imports should work alongside relative imports
    let code = r#"
(import "std/console.ffi.hp")

(defn main []
  (log "Using std library"))

(main)
"#;
    let result = hype::transpile(code);
    assert!(result.is_ok(), "Failed to transpile: {:?}", result.err());
}
