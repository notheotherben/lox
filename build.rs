use std::path::PathBuf;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=tests");
    
    build_test_package("data");
    build_test_package("lang");
}

fn build_test_package(package_name: &str) {
    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    
    let mut tests = Vec::new();
    
    for path in glob::glob(format!("tests/{}/**/*.lox", package_name).as_str()).expect("Failed to read glob pattern").flatten() {
        if let Some(name) = format!("{}", path.display()).replace(std::path::MAIN_SEPARATOR_STR, "_").strip_prefix("tests_").map(|s| s.strip_suffix(".lox").unwrap_or_default()).map(|s| s.to_owned()) {
            tests.push((path.to_owned(), name));
        }
    }

    let test_defs = tests.iter()
        .map(|(path, name)| format!("#[test]\nfn {name}() {{ run_file(\"{}\").expect(\"no errors\") }}", path.display()))
        .collect::<Vec<_>>()
        .join("\n\n");

    println!("cargo::rerun-if-changed=tests/{package_name}/");
    std::fs::create_dir_all(out_dir.join("tests")).expect("Unable to create test directory");
    std::fs::write(out_dir.join("tests").join(format!("{package_name}.rs")), test_defs).expect("Unable to write test file")
}