# Conventions

How code and prose are written here. It is shared by every project built from
[project-templates](https://github.com/thiagotjuliao/project-templates) and
changes there first, so these rules mean the same thing everywhere.

The project's own `CLAUDE.md` wins where the two disagree: a project that
writes its comments in Portuguese, or keeps the author as the only one who
writes source code, says so there, and that is the rule for that project.

## Writing

- **Artifacts are in English** — code, comments, documentation, commit
  messages — unless the project's `CLAUDE.md` says otherwise.
- **A comment says why, not what.** The code already says what it does; the
  comment carries what the code cannot: why this approach, why this order, what
  the obvious alternative breaks. Keep it short, and when the explanation needs
  more room, point to the document that has it instead of repeating it.
- **One source of truth.** A fact written in two places will disagree in one of
  them. Write it once and link to it.
- **Nothing is asserted without being checked.** A number, a benchmark, a
  compiler message, a version: run it, then write it down from the output — and
  quote messages verbatim, since the exact text is how they get searched for
  later.

## Changes

- **Tests come with the change**, not after it. A test is only worth keeping if
  it fails without the change: run it against the old code once and watch it go
  red.
- **Green means everything ran.** A run that skipped the tests it was meant to
  run is not green. Use the project's full gate before calling something done.
- **Decisions and surprises are recorded** where the next reader will look —
  the project's history, a pitfalls list, a comment next to the code — with the
  date and the measurement behind them.

## Git

- **`main` always passes the project's gate** — its own definition of green,
  which may deliberately leave red what is meant to be red, like unsolved
  exercises. Nothing is committed to `main` directly: every change arrives
  through a branch. The only exception is a repository's first commit.
- **A branch starts from the latest `origin/main`** and is named for what it
  does, in lowercase kebab-case: `align-templates`, `chapter-05`,
  `fix-lexer-escapes`.
- **One change per branch, one purpose per commit.** Work in progress that
  belongs to something else stays out of the commit, even if it sits in the
  same working tree.
- **Commit messages**: a summary line in the imperative, under about 72
  characters, in the repository's existing style (look at `git log` — some
  repositories prefix a scope, `build:` or `docs:`); then, after a blank line,
  a body that says what changed, why, and how it was verified.
- **The author is the person; the agent co-authors.** Commits are authored
  with the owner's own name and email, and a commit written with an AI agent's
  help carries the agent's `Co-Authored-By` trailer.
- **Every repository has a `LICENSE.md`**: MIT, with
  `Copyright (c) <year> Name <email>` — the same name and email the commits
  are authored with.
- **Merge with a merge commit, never a squash.** Squashing throws away the
  branch's commits, and a tag pointing at one of them falls out of `main`'s
  history.
- **Tags are annotated** (`git tag -a`), and name a state worth returning to: a
  release, a finished chapter.
- **Published history is not rewritten.** No force-push to a shared branch, no
  amending a pushed commit; a mistake is fixed by a new commit, or undone with
  `git revert`.
- **A merged branch is deleted**, locally and on the remote — after checking
  that it is contained in `origin/main`.

## Scripts

- **Scripts come in pairs**: `.sh` for bash (macOS, Linux, Git Bash) and `.ps1`
  for PowerShell 7, doing the same thing with the same arguments.
- **A script never destroys what it cannot recreate.** It creates what is
  missing, reports what differs, and leaves the decision to a person.

## Haskell

- **`-Wall` clean.** The package's `common warnings` stanza is on for every
  component; a warning is fixed, not suppressed with a pragma.
- **Explicit export lists** on every module (`module Lib (greet) where`): the
  list is the module's interface, and everything else is free to change.
- **Total functions.** No `head`, `tail`, `fromJust` or `!!` on values that can
  be empty; pattern match, or return `Maybe`/`Either`. A partial function that
  is genuinely safe says why in a comment next to it.
- **Types before code.** Write the signature first, including for local
  definitions that are not obvious; let the type checker confirm the idea
  before the body exists.
- **Formatting is fourmolu's**, from `fourmolu.yaml`, applied on save and
  checked in CI. hlint hints are followed unless `.hlint.yaml` turns one off
  with the reason next to it.
- **Tests** are an `exitcode-stdio-1.0` suite run with `cabal test all`; a
  library is added to the test suite only when the base-only runner stops being
  enough.
