use std::io::Write;

use lox::{errors, LoxError, vm::VM};

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
    let content = std::str::from_utf8(&content).map_err(|_e| errors::system(
        "The file you provided is not a valid UTF-8 file.",
        "Make sure that the file is a valid UTF-8 file.",
    ))?;

    let mut vm = VM::default();
    run(content, &mut vm)
}

fn run_prompt() -> Result<(), LoxError> {
    let mut vm = VM::default();
    let mut buffer = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush()?;

        buffer.clear();
        std::io::stdin().read_line(&mut buffer)?;

        if buffer.trim() == "exit" {
            break;
        }

        if let Err(e) = run(&buffer, &mut vm) {
            eprintln!("{}", e);
        }
    }

    Ok(())
}

fn run(source: &str, vm: &mut VM) -> Result<(), LoxError> {
    let lexer = lox::lexer::Scanner::new(source);
    let mut had_error = false;

    let (stmts, errs) = lox::ast::Parser::parse(&mut lexer.inspect(|t| if let Err(e) = t {
        eprintln!("{}", e);
        had_error = true;
    }).filter_map(|t| t.ok()));

    for err in errs {
        eprintln!("{}", err);
        had_error = true;
    }

    if had_error {
        return Err(errors::user("Errors were found in the provided source code, it will not be executed."))
    }

    let chunk = lox::compiler::compile(&stmts)?;
    vm.call(chunk)
}