# Working in this repository

Studies for the Haskell deep-dive plan in `docs/plan.md`: `core/` holds phases
1 and 2 on `base` alone (with the hand-rolled test runner), `production/`
phases 3 and 4 with `containers` and `text`, one test suite per library.

How code and prose are written here is shared with every project built from
project-templates, in `CONVENTIONS.md`. Where it and this file disagree, this
file wins.

@CONVENTIONS.md

## Commands

| command | |
| --- | --- |
| `cabal build all` | build every component |
| `cabal test all` | every test suite — the gate |
| `ghcid --command "ghci -Wall -icore -itest/core test/core/Main.hs" --test ":main"` | rerun the core tests on every save |

`ghcid` loads the sources straight from `core/` and `test/core/`, so edits to
either are picked up; `cabal repl core-tests` would load `core` as a prebuilt
package and miss them. That holds while `core` depends only on `base`.

## This project

- Each plan module gets its own namespace (`M01.*`, `M02.*`, ...), created when
  it is reached.
- Phases 1 and 2 use `base` only — the point is to build the pieces by hand.
