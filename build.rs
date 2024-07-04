use std::path::PathBuf;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=tests");
    
    build_test_package("data");
    build_test_package("lang");

    build_bench_package();
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


fn build_bench_package() {
    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    
    let mut benches = Vec::new();
    
    for path in glob::glob("benches/*.lox").expect("Failed to read glob pattern").flatten() {
        if let Some(name) = format!("{}", path.display()).replace(std::path::MAIN_SEPARATOR_STR, "_").strip_prefix("benches_").map(|s| s.strip_suffix(".lox").unwrap_or_default()).map(|s| s.to_owned()) {
            benches.push((path.to_owned(), name));
        }
    }

    let bench_defs = benches.iter()
        .map(|(path, name)| format!("#[bench]\nfn {name}(b: &mut Bencher) {{ run_file(b, \"{}\").unwrap_or_else(|e| panic!(\"Failed with error: {{}}\", e)) }}", path.display()))
        .collect::<Vec<_>>()
        .join("\n\n");

    println!("cargo::rerun-if-changed=benches/");
    std::fs::create_dir_all(out_dir.join("benches")).expect("Unable to create benches directory");
    std::fs::write(out_dir.join("benches").join("all.rs"), bench_defs).expect("Unable to write benches file")
}