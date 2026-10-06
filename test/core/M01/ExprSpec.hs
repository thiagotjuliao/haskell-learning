-- | Tests for "M01.Expr": values, both failures, and which failure wins when
-- an expression has more than one — the leftmost, as '<*>' gives for the
-- other operators, so 'Div' is held to the same order.
module M01.ExprSpec (tests) where

import M01.Expr (Env, Error (..), Expr (..), eval, fromList)
import Testing.Runner

env :: Env
env = fromList [("x", 10), ("y", 32), ("z", 0)]

tests :: [TestGroup]
tests =
  [ group
      "eval: literals and variables"
      [ assertEqual "literal" (Right 7) (eval env (Lit 7))
      , assertEqual "bound variable" (Right 10) (eval env (Var "x"))
      , assertEqual "unbound variable" (Left (VarNotFound "w")) (eval env (Var "w"))
      , assertEqual "first binding wins" (Right 1) (eval (fromList [("x", 1), ("x", 2)]) (Var "x"))
      ]
  , group
      "eval: arithmetic"
      [ assertEqual "add" (Right 42) (eval env (Add (Var "x") (Var "y")))
      , assertEqual "sub" (Right (-22)) (eval env (Sub (Var "x") (Var "y")))
      , assertEqual "mul" (Right 320) (eval env (Mul (Var "x") (Var "y")))
      , assertEqual "div" (Right 3) (eval env (Div (Var "y") (Var "x")))
      , assertEqual "div rounds down" (Right (-4)) (eval env (Div (Lit (-7)) (Lit 2)))
      , assertEqual
          "nested"
          (Right 38)
          (eval env (Add (Mul (Var "x") (Lit 3)) (Div (Var "y") (Lit 4))))
      ]
  , group
      "eval: division by zero"
      [ assertEqual "literal zero" (Left DivisionByZero) (eval env (Div (Var "x") (Lit 0)))
      , assertEqual "variable zero" (Left DivisionByZero) (eval env (Div (Var "x") (Var "z")))
      , assertEqual
          "zero from a subexpression"
          (Left DivisionByZero)
          (eval env (Div (Lit 1) (Sub (Var "x") (Lit 10))))
      , assertEqual
          "inside a larger expression"
          (Left DivisionByZero)
          (eval env (Add (Lit 1) (Div (Var "y") (Var "z"))))
      ]
  , group
      "eval: leftmost error wins"
      [ assertEqual "add" (Left (VarNotFound "a")) (eval env (Add (Var "a") (Var "b")))
      , assertEqual "div, both unbound" (Left (VarNotFound "a")) (eval env (Div (Var "a") (Var "b")))
      , assertEqual "div, unbound over zero" (Left (VarNotFound "a")) (eval env (Div (Var "a") (Lit 0)))
      , assertEqual
          "div, zero over unbound"
          (Left DivisionByZero)
          (eval env (Add (Div (Lit 1) (Lit 0)) (Var "a")))
      ]
  ]
