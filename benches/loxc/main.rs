#![feature(test)]
extern crate test;

use lox::{LoxError, errors};
use test::Bencher;

fn run_file(b: &mut Bencher, path: &str) -> Result<(), LoxError> {
    println!("Running {}", path);

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

    let chunk = lox::compiler::compile(&stmts).expect("no errors");

    b.iter(move || lox::vm::VM::default().run_function(chunk.clone()).expect("no errors"));
    Ok(())
}


include!(concat!(env!("OUT_DIR"), "/benches/all.rs"));
