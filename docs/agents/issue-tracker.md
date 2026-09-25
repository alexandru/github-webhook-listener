# Issue tracker: GitHub

Issues and specs for this repository live in GitHub Issues. Use the `gh` CLI. Run it inside this repository so it selects the repository from the Git remote.

## Issue operations

- Create: `gh issue create --title "..." --body "..."`. Use a heredoc for multiline bodies.
- Read: `gh issue view <number> --comments`. Fetch labels and filter comments with `jq` when needed.
- List: `gh issue list --state open --json number,title,body,labels,comments`, with appropriate state and label filters.
- Comment: `gh issue comment <number> --body "..."`.
- Add or remove a label: `gh issue edit <number> --add-label "..."` or `--remove-label "..."`.
- Close: `gh issue close <number> --comment "..."`.

When a skill says to publish an issue or spec, create a GitHub issue. When it says to fetch a ticket, use `gh issue view <number> --comments`.

## Pull requests as a triage surface

PRs as a request surface: no. Set this to `yes` here if the repository later treats external PRs as feature requests.

When enabled, use `gh pr view <number> --comments`, `gh pr diff <number>`, and the corresponding `gh pr list`, `gh pr comment`, `gh pr edit`, and `gh pr close` commands. Include only external authors when listing PRs for triage. A bare `#<number>` can refer to either an issue or a PR; check the PR first, then the issue.

## Ticket relationships

For a map with child tickets, use one GitHub issue as the map and GitHub sub-issues for its children. If sub-issues are unavailable, keep a task list in the map and add `Part of #<map>` to each child.

Use GitHub issue dependencies for blocking relationships. The dependency API needs the blocker's database `id`, not its issue number. If dependencies are unavailable, put `Blocked by: #<number>` at the top of the child issue.
