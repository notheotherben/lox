use std::{rc::Rc, sync::RwLock};

use lox::{LoxError, errors};

#[derive(Debug, Clone)]
struct OutputCapture {
    into: Rc<RwLock<String>>,
}

impl OutputCapture {
    pub fn new() -> Self {
        Self {
            into: Rc::new(RwLock::new(String::default())),
        }
    }

    pub fn to_string(&self) -> String {
        self.into.read().unwrap().clone()
    }
}

impl std::io::Write for OutputCapture {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let s = std::str::from_utf8(buf).unwrap();
        *self.into.write().unwrap() += s;
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn run_file(path: &str) -> Result<(), LoxError> {
    let content = std::fs::read(path)?;
    let content = std::str::from_utf8(&content).map_err(|_e| errors::system(
        "The file you provided is not a valid UTF-8 file.",
        "Make sure that the file is a valid UTF-8 file.",
    ))?;

    let expect_re = regex::RegexBuilder::new(r"//\s*expect: (.*)")
        .case_insensitive(true)
        .dot_matches_new_line(false)
        .build()
        .expect("regex should compile correctly");
    
    let expected: String = expect_re
        .captures_iter(content).map(|m| m.get(1).expect("expect expression should have a value").as_str())
        .collect::<Vec<&str>>()
        .join("\n");

    let mut errors = Vec::new();

    let lexer = lox::lexer::Scanner::new(content);
    let mut had_error = false;

    let (stmts, errs) = lox::ast::Parser::parse(&mut lexer.inspect(|t| if let Err(e) = t {
        errors.push(format!("{}", e));
        had_error = true;
    }).filter_map(|t| t.ok()));

    for err in errs {
        errors.push(format!("{}", err));
        had_error = true;
    }

    for err in lox::analysis::analyze(&stmts) {
        errors.push(format!("{}", err));
        had_error = true;
    }

    if content.contains("// Error") || content.contains("// [line ") {
        assert!(!errors.is_empty(), "Expected an error to be raised.");
    } else if !content.contains("// expect runtime error:") {
        assert!(errors.is_empty(), "Did not expect an error, got {:?}", errors);
    }

    if !had_error {
        let output = Box::new(OutputCapture::new());
        let mut interpreter = lox::interpreter::Interpreter::default().with_output(output.clone());
        for err in interpreter.interpret(&stmts) {
            errors.push(format!("{}", err));
        }

        if content.contains("// expect runtime error") {
            assert!(!errors.is_empty(), "Expected a runtime error to be raised.");
        } else {
            assert!(errors.is_empty(), "Did not expect a runtime error, got {:?}", errors);
        }

        let actual = output.to_string();
        assert_eq!(expected.trim(), actual.trim());
    }

    Ok(())
}

#[test]
fn run_suite() {
    for test_file in walkdir::WalkDir::new("tests/lang").sort_by_file_name() {
        let test_file = test_file.expect("No issues opening the test file");
        if test_file.file_type().is_file() && test_file.path().extension().map(|e| e == "lox").unwrap_or_default() {
            let path = test_file.path();
            let path = path.to_str().unwrap();
            println!("Running test: {}", path);
            run_file(path).unwrap_or_else(|e| panic!("{} failed with error: {}", path, e));
        }
    }
}