use lox::{LoxError, errors};

fn run_file(path: &str) -> Result<(), LoxError> {
    let content = std::fs::read(path)?;
    let content = std::str::from_utf8(&content).map_err(|e| errors::user_with_internal(
        "The file you provided is not a valid UTF-8 file.",
        "Make sure that the file is a valid UTF-8 file.",
        e,
    ))?;

    let lexer = lox::lexer::Scanner::new(content);
    let mut had_error = false;

    let (stmts, errs) = lox::ast::Parser::parse(&mut lexer.inspect(|t| if let Err(e) = t {
        eprintln!("{}", e);
        had_error = true;
    }).filter_map(|t| t.ok()));

    for err in errs {
        eprintln!("{}", err);
        had_error = true;
    }

    if !had_error {
        lox::interpreter::interpret(stmts)?;
        Ok(())
    } else {
        Err(errors::system(
            "An error occurred while running the provided file.", 
            "Check the output generated by the command and compare it against the expected output."))
    }
}

#[test]
fn block_scoping() {
    run_file("tests/data/block_scoping.lox").expect("no errors");
}

#[test]
fn if_conditionals() {
    run_file("tests/data/if_conditionals.lox").expect("no errors");
}

#[test]
fn fib() {
    run_file("tests/data/fib.lox").expect("no errors");
}

#[test]
fn fib_fn() {
    run_file("tests/data/fib_fn.lox").expect("no errors");
}