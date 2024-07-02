use lox::{errors, CaptureOutput, LoxError};

enum ExpectedOutput {
    NoError,
    Text(String),
    SyntaxError,
    RuntimeError,
}

fn extract_expected_output(content: &str) -> ExpectedOutput {
    let mut expected_output = String::new();

    for line in content.lines() {
        if let Some((prefix, suffix)) = line.split_once("// ") {
            if suffix.starts_with("expect:") {
                expected_output.push_str(suffix.strip_prefix("expect: ").expect("prefix is present"));
                expected_output.push('\n');
            } else if suffix.starts_with("expect runtime error:") {
                return ExpectedOutput::RuntimeError;
            } else if suffix.starts_with("Error ") {
                return ExpectedOutput::SyntaxError;
            } else if suffix.starts_with('[') && suffix.contains("] Error") {
                return ExpectedOutput::SyntaxError;
            }
        }
    }

    if !expected_output.is_empty() {
        ExpectedOutput::Text(expected_output)
    } else {
        ExpectedOutput::NoError
    }
}

fn run_file(path: &str) -> Result<(), LoxError> {
    let content = std::fs::read(path)?;
    let content = std::str::from_utf8(&content).map_err(|_e| errors::system(
        "The file you provided is not a valid UTF-8 file.",
        "Make sure that the file is a valid UTF-8 file.",
    ))?;

    let expected_output = extract_expected_output(content);

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
        let errs = lox::analysis::analyze(&stmts);
        for err in errs {
            eprintln!("{}", err);
            had_error = true;
        }
    }

    if matches!(expected_output, ExpectedOutput::SyntaxError) {
        if !had_error {
            return Err(errors::system(
                "Expected a syntax error to be present, but it was not.", 
                "Make sure that the parser or analyzer is correctly reporting this type of syntax error."));
        }
        
        return Ok(());
    }

    let out = CaptureOutput::default();
    let mut int = lox::interpreter::Interpreter::default()
        .with_output(Box::new(out.clone()));
    let errs = int.interpret(&stmts);
    for err in errs {
        eprintln!("{}", err);
        had_error = true;
    }

    match expected_output {
        ExpectedOutput::NoError if had_error => Err(errors::system(
            "Expected no errors but got some.", 
            "Check the output generated by the command and compare it against the expected output.")),
        ExpectedOutput::RuntimeError if !had_error => {
            Err(errors::system(
                "Expected an error but got none", 
                "Check the output generated by the command and compare it against the expected output."))
        },
        ExpectedOutput::Text(_) if had_error => {
            Err(errors::system(
                "Expected no errors but got some.", 
                "Check the output generated by the command and compare it against the expected output."))
        }
        ExpectedOutput::Text(output) if !had_error => {
            let actual_output = format!("{}", out);
            assert_eq!(actual_output.trim(), output.trim());

            Ok(())
        },
        _ => Ok(()),
    }
}

include!(concat!(env!("OUT_DIR"), "/tests/data.rs"));

mod lang {
    use super::run_file;

    include!(concat!(env!("OUT_DIR"), "/tests/lang.rs"));
}
