use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about = "Hype arithmetic transpiler")]
struct Args {
    /// 式を文字列で指定
    expr: String,
}

fn main() {
    let args = Args::parse();
    match hype::transpile(&args.expr) {
        Ok(js) => println!("{js}"),
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    }
}
