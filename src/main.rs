use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[derive(Parser, Debug)]
#[command(author, version, about = "Hype arithmetic transpiler")]
struct Args {
    /// 式を文字列で指定（未指定なら REPL）
    expr: Option<String>,
    /// REPL を起動
    #[arg(long)]
    repl: bool,
}

fn main() {
    let args = Args::parse();
    if args.repl || args.expr.is_none() {
        repl();
        return;
    }

    let expr = args.expr.expect("expr provided");
    match hype::transpile(&expr) {
        Ok(js) => println!("{js}"),
        Err(e) => {
            eprintln!("{}", hype::format_error(&e));
            std::process::exit(1);
        }
    }
}

fn repl() {
    let mut rl = DefaultEditor::new().expect("init readline");
    let mut buf = String::new();
    println!("Hype REPL: 空行で入力を確定、:q で終了");
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
    match hype::transpile(src) {
        Ok(js) => {
            println!("{js}");
            let _ = rl.add_history_entry(src);
        }
        Err(e) => eprintln!("{}", hype::format_error(&e)),
    }
    buf.clear();
}
