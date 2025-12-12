use std::env;

fn main() {
    let expr = env::args().nth(1).unwrap_or_else(|| {
        eprintln!("usage: hype <expression>");
        std::process::exit(1);
    });

    match hype::transpile(&expr) {
        Ok(js) => println!("{js}"),
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    }
}
