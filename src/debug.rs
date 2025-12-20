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

pub(crate) fn annotate_if_requested(
    stmts: &[crate::ast::Stmt],
    env: &TypeEnv,
    debug: Option<&mut DebugInfo>,
) {
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
    match target {
        "-" => {
            let _ = std::io::stdout().write_all(content.as_bytes());
        }
        "stderr" => {
            let _ = std::io::stderr().write_all(content.as_bytes());
        }
        path => {
            if let Err(e) = std::fs::write(path, content) {
                eprintln!("failed to write debug output to {path}: {e}");
            }
        }
    }
}

pub(crate) fn maybe_collect_debug(
    stmts: &[crate::ast::Stmt],
    env: &TypeEnv,
    debug: Option<&mut DebugInfo>,
) {
    annotate_if_requested(stmts, env, debug);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;

    #[test]
    fn writes_debug_log_to_file() {
        let mut info = DebugInfo::default();
        info.annotated_source = Some("ann".into());
        info.steps.push("step1".into());
        let mut path = env::temp_dir();
        path.push("hype_debug_test.log");
        // ensure clean
        let _ = fs::remove_file(&path);
        // set_var/remove_var are unsafe on this toolchain; limit scope.
        unsafe {
            env::set_var("HYPE_INFER_LOG", &path);
            env::remove_var("HYPE_INFER_ANNOTATED_OUT");
        }
        dump_debug_outputs(&info);
        let content = fs::read_to_string(&path).expect("file exists");
        assert!(content.contains("ann"));
        assert!(content.contains("step1"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn render_debug_text_includes_sections() {
        let mut info = DebugInfo::default();
        info.annotated_source = Some("code".into());
        info.steps.push("s1".into());
        let txt = render_debug_text(&info);
        assert!(txt.contains("== annotated =="));
        assert!(txt.contains("code"));
        assert!(txt.contains("s1"));
    }
}
