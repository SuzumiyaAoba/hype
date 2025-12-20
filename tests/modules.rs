use hype::{format_error, transpile_file};
use std::path::Path;

#[test]
fn import_ffi_module() {
    let path = Path::new("samples/main.lisp");
    let result = transpile_file(path).unwrap();

    // Should include external declarations from imported .ffi.lisp file
    assert!(result.contains("const log = console.log;"));
    assert!(result.contains("const warn = console.warn;"));
    assert!(result.contains("const error = console.error;"));

    // Should include the main function
    assert!(result.contains("function greet(name)"));
    assert!(result.contains("greet(\"World\")"));
}

#[test]
fn ffi_file_allows_external() {
    let path = Path::new("samples/console.ffi.lisp");
    let result = transpile_file(path).unwrap();

    assert!(result.contains("const log = console.log;"));
    assert!(result.contains("const warn = console.warn;"));
    assert!(result.contains("const error = console.error;"));
}

#[test]
fn lisp_file_rejects_external() {
    // Create a temporary file with external declaration (not .ffi.lisp)
    let temp_dir = std::env::temp_dir();
    let temp_file = temp_dir.join("test_external.lisp");
    std::fs::write(
        &temp_file,
        "(external log (-> String Unit) \"console.log\")",
    )
    .unwrap();

    let result = transpile_file(&temp_file);
    assert!(result.is_err());

    let err = format_error(&result.unwrap_err());
    assert!(err.contains("only allowed in .ffi"));

    // Cleanup
    let _ = std::fs::remove_file(&temp_file);
}

#[test]
fn import_file_not_found() {
    let temp_dir = std::env::temp_dir();
    let temp_file = temp_dir.join("test_import_missing.lisp");
    std::fs::write(&temp_file, "(import \"nonexistent.ffi.lisp\")").unwrap();

    let result = transpile_file(&temp_file);
    assert!(result.is_err());

    let err = format_error(&result.unwrap_err());
    assert!(err.contains("failed to read import"));

    // Cleanup
    let _ = std::fs::remove_file(&temp_file);
}
