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
ghcid --command "cabal repl core"
```
