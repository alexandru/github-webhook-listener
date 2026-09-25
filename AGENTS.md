## Agent skills

### Issue tracker

Issues and specs live in this repository's GitHub Issues. See `docs/agents/issue-tracker.md`.

### Domain docs

Use a single-context layout. See `docs/agents/domain.md`.

## Coding style

- Rust code must not panic. Do not use `unwrap`, `expect`, `panic!`, `todo!`, or `unreachable!`.
- Return `Result` and use `?` for fallible operations.
- Avoid fully qualified names in Rust code. Import types and functions where they are used; for example, use `use std::io::Result as IoResult;`.
