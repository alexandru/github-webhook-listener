# Coding standards

These rules apply to the Rust code in this repository and use RFC 2119 keywords.

Use the `rust-skills` skill when writing, reviewing, or refactoring Rust code.

## Panics

Rust code MUST NOT panic. Agents MUST NOT use `unwrap`, `expect`, `panic!`, `todo!`, or `unreachable!`.

## Errors

Fallible operations MUST return `Result` and use `?` to propagate errors.

## Imports

Agents SHOULD import types, traits, and constants freely. Agents SHOULD be more conservative about importing free functions and SHOULD import a free function only when the file uses it repeatedly. A function used once stays qualified with its module path.

```rust
use hex::FromHexError;

fn parse(input: &str) -> Result<Vec<u8>, FromHexError> {
    let bytes = hex::decode(input)?;
    // ...
    Ok(bytes)
}
```

## File structure

Files MUST start with the most important definitions: public types, their impls, and public functions. Private items follow, so the file reads from most important to least important.

## Comments

Comments MUST explain what the code does not make obvious and MUST NOT restate signatures or logic. Document the code enough for a beginner Rust developer to understand it.
