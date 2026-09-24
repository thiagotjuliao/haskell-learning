# Haskell Deep-Dive Plan

## How to use this plan

13 modules (0 to 12) in 4 phases, bottom-up: language core, built-in typeclasses, production Haskell and, finally, type-level programming.

- **`base` only:** what would normally come from a library (QuickCheck, parser combinators, transformers, lenses) is implemented from scratch. Exception: `containers` and `text` are allowed from module 7 on.
- **Theory + practice:** each module has concepts, exercises and a mini-project that wraps up the topic.
- **Tests from day one:** a hand-rolled runner in module 1 and a hand-rolled mini-QuickCheck in module 3, used from then on to test laws.
- **Progress:** tick each module's checkboxes as you go.

## Tooling: VS Code

The setup is VS Code with the Haskell extension on top of HLS, installed through GHCup. It gives autocomplete, types on hover, inline errors, HLint suggestions and in-code expression evaluation.

### Setup

1. Install GHCup (official site), which brings GHC, cabal and HLS; accept the HLS install in the installer.
2. In VS Code, install the "Haskell" extension (`haskell.haskell`). It detects GHCup and uses HLS automatically.
3. Open the repo folder in VS Code. HLS works better inside a cabal project than with loose `.hs` files.

### Commands in the integrated terminal

| Command | Purpose |
| --- | --- |
| `cabal build all` | Builds every library and test suite |
| `cabal repl core` | GHCi with the modules loaded (`:t`, `:i`, `:k`, `:r` to reload) |
| `cabal test all` | Runs the test suites (hand-rolled runner and mini-QuickCheck) |
| `ghcid --command "cabal repl core"` | Recompiles on every save and shows errors right away |

### Tips

- **`-- >>>` comments:** writing `-- >>> map (+1) [1,2,3]` above a function shows an "Evaluate" button with the result inside the file. Handy for the expressiveness exercises.
- **ghcid:** install with `cabal install ghcid` and keep it running in a separate terminal.
- **HLS failing or slow:** usually a version mismatch between GHC and HLS. Run `ghcup tui` and pick the versions marked "recommended" for both.

## Repo layout

One cabal package with two internal libraries: `core` only sees `base`, so the Phase 1 and 2 rule is enforced by the compiler.

```
haskell-learning/
├── cabal.project
├── haskell-learning.cabal
├── core/                  -- Phases 1 and 2 (modules 0–6), base only
│   ├── Testing/Runner.hs  -- hand-rolled runner (module 1)
│   ├── Testing/Check.hs   -- mini-QuickCheck (module 3)
│   └── M01/ … M06/
├── production/            -- Phases 3 and 4 (modules 7–12), + containers and text
│   └── M07/ … M12/
├── test/
│   ├── core/Main.hs
│   └── production/Main.hs
└── docs/
    └── plan.md
```

| Decision | Why |
| --- | --- |
| `core` and `production` libraries | `production` depends on `core` and reuses the hand-rolled parser, monads and QuickCheck |
| Test framework inside `core` | Module 6 refactors `Gen` to use the hand-rolled `State` |
| One namespace per module (`M01.*`, `M02.*`…) | Files are created when you reach the module, not all at once |
| `default-language: Haskell2010` | Every extension becomes a deliberate `LANGUAGE` pragma, without what `GHC2021` turns on by default |
| `-Wall -Wcompat` and friends, no `-Werror` | Warnings stay visible without blocking experimentation |
| `import Prelude hiding (…)` in module 1 | Reimplement `map`, `foldr` etc. without name clashes |

Workflow: keep `ghcid --command "cabal repl core"` open while studying, `cabal test all` for the suite, and one commit per exercise or mini-project.

## Phase 1 — Language foundations

### Module 0 — Tools and habits

The setup from the Tooling section above, always with `-Wall`. GHCi becomes a lab: `:t`, `:i` and `:k` to inspect types, instances and kinds. Typed holes (`_`) make the compiler tell you what is missing.

- [ ] Tooling section setup installed and working
- [ ] Study cabal project created with `-Wall`
- [ ] Habit of using typed holes and `:i` in GHCi

### Module 1 — Core expressiveness

ADTs, pattern matching, guards, `where`/`let`, `case`, currying, operator sections, composition (`.`), `$`, point-free style, list comprehensions, and the difference between `data`, `newtype` and `type`.

- [ ] Reimplement the list Prelude: `map`, `filter`, `foldr`, `foldl`, `zipWith`, `takeWhile`, `span`, `words`
- [ ] Each function three ways: explicit recursion, via `foldr` and point-free; compare readability
- [ ] Tests: hand-rolled `assertEqual` and a minimal runner that counts passes and failures
- [ ] Mini-project: arithmetic expression evaluator with variables (AST as an ADT, environment as a list of pairs)

### Module 2 — Base typeclasses

`Eq`, `Ord`, `Show`, `Read`, `Enum`, `Bounded` and the numeric hierarchy (`Num`, `Integral`, `Fractional`, `Floating`, `Real`). Concepts: superclasses, default methods, minimal complete definition, `deriving` versus manual instances, coherence and orphan instances.

- [ ] Hand-write instances that are usually derived and check them against the derived ones
- [ ] Mini-project: your own `Rational` or `Money` with a full `Num` instance
- [ ] Mini-project: 2x2 matrix with `Num` and Fibonacci by exponentiation

## Phase 2 — Algebraic hierarchy

### Module 3 — Semigroup and Monoid

Laws and the classic newtypes: `Sum`, `Product`, `Min`, `Max`, `First`, `Last`, `Any`, `All`, `Endo`, `Dual`. They exist because the same type can have several monoids.

- [ ] Hand-rolled pseudo-random generator (a simple LCG or splitmix), since `random` is not part of `base`
- [ ] Tests (milestone): hand-rolled mini-QuickCheck, with `newtype Gen a = Gen (Seed -> (a, Seed))`, an `Arbitrary` class and `forAll`
- [ ] Test the Semigroup and Monoid laws with the mini-QuickCheck
- [ ] Derive instances for your own newtypes with `GeneralizedNewtypeDeriving` and `DerivingVia`, comparing with the manual ones
- [ ] Mini-project: single-pass statistics aggregator (count, sum, min, max, mean) as one composite monoid

### Module 4 — Functor and Foldable

Kinds (`* -> *`), why `Either e` and `(,) a` are functors, and the function functor `(->) r`. In `Foldable`, `foldMap` alone derives `length`, `sum`, `elem` and `toList`.

- [ ] Instances for a binary tree and a rose tree
- [ ] Functor laws tested with the mini-QuickCheck
- [ ] Mini-project: in-memory file system (rose tree) queried only through `foldMap` and the monoids from module 3

### Module 5 — Applicative and Traversable

`pure`, `<*>`, `liftA2`; instances for Maybe, Either, list and `ZipList`. Applicative means independent effects, Monad dependent effects. Then `traverse`, `sequenceA`, `Identity` and `Const`.

- [ ] Implement `Validation` and explain why it is an Applicative but cannot be a Monad
- [ ] Mini-project: schema validator for records, using `traverse` over rows and accumulating every error

### Module 6 — Monad and the classic monads from scratch

Laws, desugaring do-notation by hand with `>>=`. Implement `Reader`, `Writer`, `State`, Maybe, Either and list. Then `Alternative`, `MonadPlus` and `MonadFail`.

- [ ] Implement Reader, Writer and State from scratch
- [ ] Refactor the mini-QuickCheck `Gen` to use your own `State`
- [ ] Mini-project: parser combinators from scratch, `newtype Parser a = Parser (String -> Maybe (a, String))`, with Functor, Applicative, Monad and Alternative
- [ ] Wrap-up: parser for a subset of JSON

## Phase 3 — Production Haskell

### Module 7 — Transformers and IO

`StateT`, `ReaderT`, `ExceptT` and `MonadTrans` from scratch. Then the mtl-style classes (`MonadState`, `MonadReader`, `MonadError`) and the n² instances problem. IO exceptions with `Control.Exception` (`throwIO`, `catch`, `try`, `bracket`) and when to use each instead of `ExceptT`. Wrap up with the ReaderT-over-IO pattern. From here on `containers` and `text` are allowed.

- [ ] Implement StateT, ReaderT, ExceptT and MonadTrans
- [ ] Implement the mtl-style classes and their instances
- [ ] Handle IO errors with `try` and release resources with `bracket`
- [ ] Mini-project: interpreter for a small imperative language (assignment, while, print) with state, errors and logging, reusing the module 6 parser and using `Data.Map` for variables

### Module 8 — Laziness and performance

Thunks, WHNF versus NF, `seq`, bang patterns, `foldl` versus `foldl'`, space leaks, infinite structures and tying the knot. Measure with `+RTS -s` and profiling.

- [ ] Cause and fix a space leak on purpose
- [ ] Mini-project: persistent queue with two lists, then a lazy queue with an amortized bound (Okasaki style)
- [ ] Benchmarks comparing the versions, measured with `getCPUTime` and `+RTS -s` (no `criterion`)

### Module 9 — Concurrency

`forkIO`, `MVar`, `IORef` and STM (`TVar`, `retry`, `orElse`). Asynchronous exceptions: `killThread`, `throwTo`, `mask` and why `bracket` matters in concurrent code.

- [ ] Mini-project: simplified `async` (`async`, `wait`, `race`, `concurrently`) on top of `MVar`, using `mask` so `race` cancels the losing thread without leaking it
- [ ] Mini-project: worker pool with an STM queue

## Phase 4 — Moving up to the type level

### Module 10 — Advanced type system

`RankNTypes`, existential types, GADTs, type families, `DataKinds`, kind polymorphism and `TypeApplications`.

- [ ] Mini-project: typed expression evaluator with GADTs, where ill-typed expressions do not even compile
- [ ] Mini-project: `Vec (n :: Nat) a` with the length in the type and a safe `head`

### Module 11 — Free monads and tagless final

Two ways to separate describing a program from running it. Free monads build the program's AST; tagless final uses typeclasses as the interface (compare with the Cats Effect style).

- [ ] Mini-project: the same key-value store DSL in Free and in tagless final, with a pure interpreter for tests and another in IO

### Module 12 — Exotic functors, Comonad and lenses

`Bifunctor`, `Contravariant`, `Profunctor`, `Category`/`Arrow` and `Comonad`.

- [ ] Mini-project: Game of Life with the zipper comonad
- [ ] Mini-project: mini van Laarhoven lens library (`Lens`, `view`, `over`, `set` via `Const` and `Identity`)

## Resources and pace

| Resource | Role in the plan |
| --- | --- |
| *Haskell Programming from First Principles* | Backbone of phases 1 and 2, same progression and lots of exercises |
| *Typeclassopedia* (Haskell wiki) | Reference for each typeclass |
| *Thinking with Types* (Sandy Maguire) | Phase 4 |
| *Parallel and Concurrent Programming in Haskell* (Simon Marlow) | Module 9 |
| Exercism (Haskell track) and Advent of Code | Free-form expressiveness practice between modules |

### Suggested pace

Estimate for 6 to 8 hours a week, about 21 to 27 weeks in total. Adjust to the real time each module takes.

| Phase | Modules | Weeks |
| --- | --- | --- |
| Phase 1 — Language foundations | 0 to 2 | 3–4 |
| Phase 2 — Algebraic hierarchy | 3 to 6 | 7–9 |
| Phase 3 — Production Haskell | 7 to 9 | 5–6 |
| Phase 4 — Moving up to the type level | 10 to 12 | 6–8 |

Do not skip modules 3 to 6 even if they look familiar. Implementing the instances and testing the laws with your own QuickCheck turns "I know how to use it" into "I understand why it works".
