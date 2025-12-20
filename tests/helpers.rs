use hype::{format_error, parse_lisp, render_program, transpile};

/// Transpile LISP code to JavaScript
pub fn transpile_lisp(input: &str) -> Result<String, String> {
    let stmts = parse_lisp(input).map_err(|e| e.message)?;
    Ok(render_program(&stmts))
}

/// Get formatted error message from failed transpilation
pub fn err(input: &str) -> String {
    let e = transpile(input).unwrap_err();
    format_error(&e)
}
