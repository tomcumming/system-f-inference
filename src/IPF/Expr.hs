module IPF.Expr
  ( Var,
    Value (..),
    Comp (..),
  )
where

import IPF.Type qualified as T

type Var = String

data Value
  = Var Var
  | Thunk Comp

data Comp
  = Abs Var T.Pos Comp
  | TAbs T.Var Comp
  | Ret Value
  | Let Var Value [Value] Comp
  | ALet Var T.Pos Value [Value] Comp
