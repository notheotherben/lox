# Lox
**A Rust implementation of the Crafting Interpreters language Lox**
This repository contains a Rust implementation of the Lox language as
described in [Crafting Compilers](https://www.craftingcompilers.com/).
It attempts to remain relatively faithful to the book's implementation,
while also taking advantage of Rust's idiomatic approach to solving certain
problems.

## Usage
This repository contains two Rust binaries: `loxi` and `loxc` which correspond
to the interpreted and compiled versions of the Lox language respectively.

```bash
# Run the loxi binary
cargo run --bin loxi

# Run the loxc binary
cargo run --bin loxc
```