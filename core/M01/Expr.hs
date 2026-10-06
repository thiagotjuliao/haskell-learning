module M01.Expr (Expr (..), Env, Error (..), fromList, eval) where

data Expr
  = Lit Integer
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

data Error
  = VarNotFound String
  | DivisionByZero
  deriving (Show, Eq)

newtype Env = Env [(String, Integer)]

fromList :: [(String, Integer)] -> Env
fromList = Env

lookupVar :: Env -> String -> Either Error Integer
lookupVar (Env []) n = Left $ VarNotFound n
lookupVar (Env ((k, v) : xs)) n
  | n == k = Right v
  | otherwise = lookupVar (Env xs) n

eval :: Env -> Expr -> Either Error Integer
eval _ (Lit a) = Right a
eval env (Var x) = lookupVar env x
eval env (Add l r) = (+) <$> eval env l <*> eval env r
eval env (Sub l r) = (-) <$> eval env l <*> eval env r
eval env (Mul l r) = (*) <$> eval env l <*> eval env r
eval
  env
  (Div l r) =
    case eval env l of
      Left e1 -> Left e1
      Right n ->
        case eval env r of
          Right 0 -> Left DivisionByZero
          Right d -> Right $ n `div` d
          Left e2 -> Left e2
