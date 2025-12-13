pub mod ast;
mod debug;
pub mod error;
mod lexer;
mod parser;
mod render;
mod typecheck;

pub use ast::{BinOp, Expr, ExprKind, FnSig, MatchArm, Pattern, Stmt, Type};
pub use debug::{render_debug_text, DebugInfo};
pub use error::{format_error, ParseError};
pub use render::render_program;

pub fn transpile(input: &str) -> Result<String, ParseError> {
    let mut debug = debug::debug_requested_from_env();
    let result = transpile_with_debug(input, debug.as_mut());
    if let (Ok(_), Some(info)) = (&result, debug.as_ref()) {
        debug::dump_debug_outputs(info);
    }
    result
}

pub fn transpile_with_debug(
    input: &str,
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
    let env = typecheck::typecheck(&stmts, input, debug.as_deref_mut())?;
    debug::maybe_collect_debug(&stmts, &env, debug.as_deref_mut());
    Ok(render::render_program(&stmts))
}
