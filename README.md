# haskell-learning

Studies for the Haskell deep-dive plan: see [docs/plan.md](docs/plan.md).

- `core/`: Phases 1 and 2 (modules 0–6), `base` only. Holds the test runner and the mini-QuickCheck.
- `production/`: Phases 3 and 4 (modules 7–12), with `containers` and `text`.
- `test/`: one test suite per library.
- `docs/`: the study plan and, later, notes per module.

Each plan module gets its own namespace (`M01.*`, `M02.*`…), created when you reach it.

```bash
cabal build all
cabal test all
ghcid --command "ghci -Wall -icore -itest/core test/core/Main.hs" --test ":main"
```

The `ghcid` line reloads on every save and reruns the core tests. It loads the sources straight from `core/` and `test/core/`, so edits to either are picked up; `cabal repl core-tests` would load `core` as a prebuilt package and miss them. This works while `core` depends only on `base`.
