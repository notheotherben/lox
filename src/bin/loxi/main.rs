use std::io::Write;

use lox::{cmdline::CommandLineOptions, errors, interpreter::Interpreter, LoxError};

fn main() {
    let opts = CommandLineOptions::parse();

    let mut interpreter = Interpreter::default();

    let result = if let Some(file) = opts.file {
        run_file(&file, &mut interpreter)
    } else {
        run_prompt(&mut interpreter)
    };

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn run_file(filename: &str, interpreter: &mut Interpreter) -> Result<(), LoxError> {
    let content = std::fs::read(filename)?;
    let content = std::str::from_utf8(&content).map_err(|_e| errors::system(
        "The file you provided is not a valid UTF-8 file.",
        "Make sure that the file is a valid UTF-8 file.",
    ))?;

    run(content, interpreter)
}

fn run_prompt(interpreter: &mut Interpreter) -> Result<(), LoxError> {
    let mut buffer = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush()?;

        buffer.clear();
        std::io::stdin().read_line(&mut buffer)?;

        if buffer.trim() == "exit" {
            break;
        }

        if let Err(e) = run(&buffer, interpreter) {
            eprintln!("{}", e);
        }
    }

    Ok(())
}

fn run(source: &str, interpreter: &mut Interpreter) -> Result<(), LoxError> {
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
    
    let errs = lox::analysis::analyze(&stmts);
    for err in errs {
        eprintln!("{}", err);
        had_error = true;
    }

    if !had_error {
        let errs = interpreter.interpret(&stmts);
        for err in errs {
            eprintln!("{}", err);
        }
    }

    Ok(())
}