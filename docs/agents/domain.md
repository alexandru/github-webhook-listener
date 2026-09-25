# Domain docs

Read the relevant domain docs before exploring code or naming domain concepts.

## What to read

- Read root `CONTEXT.md` if it exists. If the repository has a `CONTEXT-MAP.md`, follow it to the relevant context's `CONTEXT.md` instead.
- Read decisions in `docs/adr/` that apply to the area you are changing. In a multi-context repository, also check `src/<context>/docs/adr/`.

Proceed if any of these files or directories do not exist. Do not create them merely because they are absent; the domain-modeling skill creates them when terminology or decisions need recording.

## Layout

This repository uses one context: root `CONTEXT.md` for the glossary and `docs/adr/` for decisions. These files can be created when needed. A future multi-context layout would use root `CONTEXT-MAP.md` and context-specific `CONTEXT.md` files.

## Terminology and decisions

Use the terms in `CONTEXT.md` in issue titles, tests, and proposals. If a needed term is missing, check whether the code already has a term for it before adding a new one.

Call out any proposal that conflicts with an existing ADR rather than silently overriding the decision.
