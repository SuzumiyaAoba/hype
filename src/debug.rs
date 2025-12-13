use std::io::Write;

use crate::render::render_annotated_program;
use crate::typecheck::TypeEnv;

#[derive(Debug, Default, Clone)]
pub struct DebugInfo {
    pub steps: Vec<String>,
    pub annotated_source: Option<String>,
}

/// デバッグ出力の組み立て（テキスト化）。スナップショットテストでも利用する。
pub fn render_debug_text(info: &DebugInfo) -> String {
    let mut buf = String::new();
    if let Some(annot) = &info.annotated_source {
        buf.push_str("== annotated ==\n");
        buf.push_str(annot);
        buf.push_str("\n\n");
    }
    buf.push_str("== steps ==\n");
    for step in &info.steps {
        buf.push_str(step);
        buf.push('\n');
    }
    buf
}

pub(crate) fn log_debug<F: FnOnce() -> String>(debug: Option<&mut DebugInfo>, f: F) {
    if let Some(d) = debug {
        d.steps.push(f());
    }
}

pub(crate) fn annotate_if_requested(stmts: &[crate::ast::Stmt], env: &TypeEnv, debug: Option<&mut DebugInfo>) {
    if let Some(d) = debug {
        d.annotated_source = Some(render_annotated_program(stmts, env));
    }
}

pub(crate) fn debug_requested_from_env() -> Option<DebugInfo> {
    let has_log = std::env::var("HYPE_INFER_LOG")
        .map(|v| !v.is_empty())
        .unwrap_or(false);
    let has_annot = std::env::var("HYPE_INFER_ANNOTATED_OUT")
        .map(|v| !v.is_empty())
        .unwrap_or(false);
    if has_log || has_annot {
        Some(DebugInfo::default())
    } else {
        None
    }
}

pub(crate) fn dump_debug_outputs(info: &DebugInfo) {
    if let Ok(path) = std::env::var("HYPE_INFER_LOG") {
        if !path.is_empty() {
            let text = render_debug_text(info);
            write_output(&path, &text);
        }
    }
    if let Ok(path) = std::env::var("HYPE_INFER_ANNOTATED_OUT") {
        if !path.is_empty() {
            if let Some(annot) = &info.annotated_source {
                write_output(&path, annot);
            }
        }
    }
}

fn write_output(target: &str, content: &str) {
    if target == "-" {
        let _ = std::io::stdout().write_all(content.as_bytes());
    } else if let Err(e) = std::fs::write(target, content) {
        eprintln!("failed to write debug output to {target}: {e}");
    }
}

pub(crate) fn maybe_collect_debug(
    stmts: &[crate::ast::Stmt],
    env: &TypeEnv,
    debug: Option<&mut DebugInfo>,
) {
    annotate_if_requested(stmts, env, debug);
}
