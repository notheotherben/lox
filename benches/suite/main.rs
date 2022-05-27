#![feature(test)]
extern crate test;

use lox::{LoxError, errors};
use test::Bencher;

fn run_file(b: &mut Bencher, path: &str) -> Result<(), LoxError> {
    let content = std::fs::read(path)?;
    let content = std::str::from_utf8(&content).map_err(|_e| errors::system(
        "The file you provided is not a valid UTF-8 file.",
        "Make sure that the file is a valid UTF-8 file.",
    ))?;

    let lexer = lox::lexer::Scanner::new(content);

    let mut tokens = Vec::new();
    for token in lexer {
        tokens.push(token?);
    }

    let (stmts, errs) = lox::ast::Parser::parse(&mut tokens.into_iter());

    if let Some(err) = errs.into_iter().next() {
        return Err(err);
    }

    b.iter(move || lox::interpreter::interpret(stmts.clone()));
    Ok(())
}

#[bench]
fn loxi(b: &mut Bencher) {
    for test_file in walkdir::WalkDir::new("benches/suite").sort_by_file_name() {
        let test_file = test_file.expect("No issues opening the test file");
        if test_file.file_type().is_file() && test_file.path().extension().map(|e| e == "lox").unwrap_or_default() {
            let path = test_file.path();
            let path = path.to_str().unwrap();
            run_file(b, path).unwrap_or_else(|e| panic!("{} failed with error: {}", path, e));
        }
    }
}