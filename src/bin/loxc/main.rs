use lox::{errors, LoxError, vm::VM};
use lox::cmdline::CommandLineOptions;

fn main() {
    let opts = CommandLineOptions::parse();

    let mut vm = VM::default();
    if opts.debug {
        vm = vm.with_debug();
    }

    let result = if let Some(file) = opts.file {
        run_file(&file, vm)
    } else {
        eprintln!("REPL is only supported in the loxi binary.");
        std::process::exit(1);
    };

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn run_file(filename: &str, vm: VM) -> Result<(), LoxError> {
    let content = std::fs::read(filename)?;
    let content = std::str::from_utf8(&content).map_err(|_e| errors::system(
        "The file you provided is not a valid UTF-8 file.",
        "Make sure that the file is a valid UTF-8 file.",
    ))?;

    run(content, vm)
}

fn run(source: &str, vm: VM) -> Result<(), LoxError> {
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

    if had_error {
        return Err(errors::user("Errors were found in the provided source code, it will not be executed."))
    }

    let chunk = lox::compiler::compile(&stmts)?;
    vm.run_function(chunk)
}