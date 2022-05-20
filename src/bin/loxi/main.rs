use std::io::Write;

use lox::{errors, LoxError};

fn main() {
    let result = match std::env::args().nth(1) {
        Some(filename) => {
            run_file(&filename)
        },
        None => {
            run_prompt()
        }
    };

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn run_file(filename: &str) -> Result<(), LoxError> {
    let content = std::fs::read(filename)?;
    let content = std::str::from_utf8(&content).map_err(|e| errors::user_with_internal(
        "The file you provided is not a valid UTF-8 file.",
        "Make sure that the file is a valid UTF-8 file.",
        e,
    ))?;

    run(content)
}

fn run_prompt() -> Result<(), LoxError> {
    let mut buffer = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush()?;

        buffer.clear();
        std::io::stdin().read_line(&mut buffer)?;

        if buffer.trim() == "exit" {
            break;
        }

        if let Err(e) = run(&buffer) {
            eprintln!("{}", e);
        }
    }

    Ok(())
}

fn run(source: &str) -> Result<(), LoxError> {
    let lexer = lox::lexer::Scanner::new(source);
    let mut had_error = false;

    let stmts = lox::ast::Parser::parse(&mut lexer.inspect(|t| if let Err(e) = t {
        eprintln!("{}", e);
        had_error = true;
    }).filter_map(|t| t.ok()))?;

    if !had_error {
        lox::interpreter::interpret(stmts)?;
    }

    Ok(())
}