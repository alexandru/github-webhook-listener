This file uses RFC 2119 keywords (MUST, MUST NOT, SHOULD, SHOULD NOT, MAY) to state requirement levels.

## Agent skills

### Issue tracker

Issues and specs for this repository MUST live in GitHub Issues. Agents MUST use `docs/agents/issue-tracker.md` for tracker operations.

### Domain docs

Agents SHOULD read `docs/agents/domain.md` before exploring the codebase or naming domain concepts.

## Coding style

- Rust code MUST NOT panic. Agents MUST NOT use `unwrap`, `expect`, `panic!`, `todo!`, or `unreachable!`.
- Fallible operations MUST return `Result` and use `?`.
- Rust code SHOULD import the names it uses instead of fully qualified paths. For example, use `use std::io::Result as IoResult;`.
- Agents MUST NOT modify `README.md` without the user's permission. Reference updates and typo fixes are the only exceptions.
