pub mod ast;
mod debug;
pub mod error;
mod lexer;
pub mod lisp;
mod parser;
mod render;
mod typecheck;

use std::collections::HashSet;
use std::path::{Path, PathBuf};

pub use ast::{BinOp, Expr, ExprKind, FnSig, MatchArm, Pattern, Stmt, Type};
pub use debug::{render_debug_text, DebugInfo};
pub use error::{format_error, ParseError};
pub use render::render_program;

/// Source context determines how `external` declarations are handled
#[derive(Debug, Clone)]
pub enum SourceContext {
    /// From REPL - warn on external but allow
    Repl,
    /// From a file - check extension for external permission
    File(PathBuf),
    /// From string input - allow external (for testing/programmatic use)
    String,
}

impl SourceContext {
    /// Check if external declarations are allowed
    pub fn allows_external(&self) -> bool {
        match self {
            SourceContext::Repl => true,   // allowed with warning
            SourceContext::String => true, // allowed for testing/programmatic use
            SourceContext::File(path) => {
                path.to_string_lossy().ends_with(".ffi.hp")
            }
        }
    }

    /// Check if external should produce a warning
    pub fn warns_on_external(&self) -> bool {
        matches!(self, SourceContext::Repl)
    }
}

pub fn transpile(input: &str) -> Result<String, ParseError> {
    let mut debug = debug::debug_requested_from_env();
    let result = transpile_with_debug(input, debug.as_mut());
    if let (Ok(_), Some(info)) = (&result, debug.as_ref()) {
        debug::dump_debug_outputs(info);
    }
    result
}

/// Transpile a file with import resolution
pub fn transpile_file(path: &Path) -> Result<String, ParseError> {
    let mut debug = debug::debug_requested_from_env();
    let result = transpile_file_with_debug(path, debug.as_mut());
    if let (Ok(_), Some(info)) = (&result, debug.as_ref()) {
        debug::dump_debug_outputs(info);
    }
    result
}

pub fn transpile_file_with_debug(
    path: &Path,
    debug: Option<&mut DebugInfo>,
) -> Result<String, ParseError> {
    let input = std::fs::read_to_string(path).map_err(|e| ParseError {
        message: format!("failed to read file: {}", e),
        span: 0..0,
        source: String::new(),
    })?;
    let context = SourceContext::File(path.to_path_buf());
    transpile_with_context(&input, context, Some(path), debug)
}

/// Transpile from REPL with external warnings
pub fn transpile_repl(input: &str) -> Result<String, ParseError> {
    let mut debug = debug::debug_requested_from_env();
    let result = transpile_with_context(input, SourceContext::Repl, None, debug.as_mut());
    if let (Ok(_), Some(info)) = (&result, debug.as_ref()) {
        debug::dump_debug_outputs(info);
    }
    result
}

pub fn transpile_with_debug(
    input: &str,
    debug: Option<&mut DebugInfo>,
) -> Result<String, ParseError> {
    transpile_with_context(input, SourceContext::String, None, debug)
}

fn transpile_with_context(
    input: &str,
    context: SourceContext,
    base_path: Option<&Path>,
    mut debug: Option<&mut DebugInfo>,
) -> Result<String, ParseError> {
    let tokens = lexer::lex(input)?;
    let stmts = match parser::parse(tokens) {
        Ok(s) => s,
        Err(mut e) => {
            if e.source.is_empty() {
                e.source = input.to_string();
            }
            return Err(e);
        }
    };

    // Validate external usage based on context
    validate_external_usage(&stmts, &context, input)?;

    // Resolve imports
    let mut loaded = HashSet::new();
    if let Some(p) = base_path {
        loaded.insert(p.canonicalize().unwrap_or_else(|_| p.to_path_buf()));
    }
    let resolved_stmts = resolve_imports(stmts, base_path, &mut loaded)?;

    let env = typecheck::typecheck(&resolved_stmts, input, debug.as_deref_mut())?;
    debug::maybe_collect_debug(&resolved_stmts, &env, debug.as_deref_mut());
    Ok(render::render_program(&resolved_stmts))
}

/// Validate that external declarations are only used in allowed contexts
fn validate_external_usage(
    stmts: &[Stmt],
    context: &SourceContext,
    source: &str,
) -> Result<(), ParseError> {
    for stmt in stmts {
        if let Stmt::External { name, .. } = stmt {
            if !context.allows_external() {
                return Err(ParseError {
                    message: format!(
                        "`external` declaration `{}` is only allowed in .ffi.hp files",
                        name
                    ),
                    span: 0..0,
                    source: source.to_string(),
                });
            }
            if context.warns_on_external() {
                eprintln!(
                    "warning: `external` declaration `{}` in REPL; consider using .ffi.hp file",
                    name
                );
            }
        }
    }
    Ok(())
}

/// Resolve imports by loading and merging imported files
fn resolve_imports(
    stmts: Vec<Stmt>,
    base_path: Option<&Path>,
    loaded: &mut HashSet<PathBuf>,
) -> Result<Vec<Stmt>, ParseError> {
    let mut result = Vec::new();

    for stmt in stmts {
        match stmt {
            Stmt::Import { path } => {
                let import_path = resolve_import_path(&path, base_path)?;
                let canonical = import_path
                    .canonicalize()
                    .unwrap_or_else(|_| import_path.clone());

                // Skip if already loaded (prevent circular imports)
                if loaded.contains(&canonical) {
                    continue;
                }
                loaded.insert(canonical);

                // Load and parse the imported file
                let content = std::fs::read_to_string(&import_path).map_err(|e| ParseError {
                    message: format!("failed to read import '{}': {}", path, e),
                    span: 0..0,
                    source: String::new(),
                })?;

                let tokens = lexer::lex(&content)?;
                let imported_stmts = parser::parse(tokens).map_err(|mut e| {
                    if e.source.is_empty() {
                        e.source = content.clone();
                    }
                    e.message = format!("in import '{}': {}", path, e.message);
                    e
                })?;

                // Validate external usage in imported file
                let import_context = SourceContext::File(import_path.clone());
                validate_external_usage(&imported_stmts, &import_context, &content)?;

                // Recursively resolve imports
                let resolved = resolve_imports(imported_stmts, Some(&import_path), loaded)?;

                // Add imported definitions (exclude Stmt::Expr from imports)
                for s in resolved {
                    match &s {
                        Stmt::Expr(_) => {} // Skip top-level expressions from imports
                        _ => result.push(s),
                    }
                }
            }
            other => result.push(other),
        }
    }

    Ok(result)
}

/// Resolve import path relative to the current file
fn resolve_import_path(import: &str, base_path: Option<&Path>) -> Result<PathBuf, ParseError> {
    let import_path = Path::new(import);

    if import_path.is_absolute() {
        return Ok(import_path.to_path_buf());
    }

    match base_path {
        Some(base) => {
            let dir = base.parent().unwrap_or(Path::new("."));
            Ok(dir.join(import_path))
        }
        None => Err(ParseError {
            message: format!(
                "cannot resolve relative import '{}' without base path",
                import
            ),
            span: 0..0,
            source: String::new(),
        }),
    }
}
