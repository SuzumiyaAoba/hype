use clap::Parser;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::path::Path;

#[derive(Parser, Debug)]
#[command(
    author,
    version,
    about = "Hype - LISP-style functional language transpiler"
)]
struct Args {
    /// LISP式を文字列で指定、または .lisp ファイルパス（未指定なら REPL）
    expr: Option<String>,
    /// REPL を起動
    #[arg(long)]
    repl: bool,
    /// 推論デバッグの出力先（\"-\"=stdout, \"stderr\"=標準エラー, それ以外はファイルパス）
    #[arg(long)]
    infer_debug_log: Option<String>,
    /// 推論注釈付きソースの出力先（\"-\"=stdout, \"stderr\"=標準エラー, それ以外はファイルパス）
    #[arg(long)]
    infer_annotated_out: Option<String>,
}

fn main() {
    let args = Args::parse();

    unsafe {
        if let Some(path) = &args.infer_debug_log {
            std::env::set_var("HYPE_INFER_LOG", path);
        }
        if let Some(path) = &args.infer_annotated_out {
            std::env::set_var("HYPE_INFER_ANNOTATED_OUT", path);
        }
    }

    if args.repl || args.expr.is_none() {
        repl();
        return;
    }

    let expr = args.expr.expect("expr provided");

    // Check if input is a file path (.lisp)
    let path = Path::new(&expr);
    if path.extension().is_some_and(|e| e == "lisp") || path.exists() {
        match hype::transpile_file(path) {
            Ok(js) => println!("{js}"),
            Err(e) => {
                eprintln!("{}", hype::format_error(&e));
                std::process::exit(1);
            }
        }
    } else {
        match hype::transpile(&expr) {
            Ok(js) => println!("{js}"),
            Err(e) => {
                eprintln!("{}", hype::format_error(&e));
                std::process::exit(1);
            }
        }
    }
}

fn repl() {
    let mut rl = DefaultEditor::new().expect("init readline");
    let mut buf = String::new();
    println!("Hype REPL (LISP syntax): 空行で入力を確定、:q で終了");
    println!("例: (+ 1 2), (defn add [x y] (+ x y))");
    loop {
        let prompt = if buf.is_empty() { "> " } else { "| " };
        match rl.readline(prompt) {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed == ":q" || trimmed == ":quit" {
                    break;
                }
                if buf.is_empty() {
                    buf.push_str(&line);
                } else {
                    buf.push('\n');
                    buf.push_str(&line);
                }
                if trimmed.is_empty() && !buf.trim().is_empty() {
                    run_buffer(&mut rl, &mut buf);
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                if !buf.trim().is_empty() {
                    run_buffer(&mut rl, &mut buf);
                }
                break;
            }
            Err(e) => {
                eprintln!("readline error: {e}");
                break;
            }
        }
    }
}

fn run_buffer(rl: &mut DefaultEditor, buf: &mut String) {
    let src = buf.trim();
    match hype::transpile_repl(src) {
        Ok(js) => {
            println!("{js}");
            let _ = rl.add_history_entry(src);
        }
        Err(e) => eprintln!("{}", hype::format_error(&e)),
    }
    buf.clear();
}
