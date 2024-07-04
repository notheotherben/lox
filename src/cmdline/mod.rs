#[derive(Debug, Default, Clone)]
pub struct CommandLineOptions {
    pub file: Option<String>,
    pub debug: bool,
}

impl CommandLineOptions {
    pub fn parse() -> Self {
        let mut options = CommandLineOptions::default();

        for arg in std::env::args().skip(1) {
            match arg.as_str() {
                "-d" | "--debug" => options.debug = true,
                _ => {
                    if options.file.is_none() {
                        options.file = Some(arg);
                    } else {
                        eprintln!("Ignoring unrecognized command line argument: {}", arg);
                    }
                }
            }
        }

        options
    }
}