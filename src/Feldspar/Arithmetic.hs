module Feldspar.Arithmetic
  ( arithmetic
  ) where

import Feldspar.AST (pattern (:!), pattern (:+), pattern (:*), bottomUpExpr, Expr, pattern I32, Name(..), pattern Poison)
import Feldspar.Intrinsic (Intrinsic(..), ZeroDivisionPolicy(..))
import Prelude
import Zabt (pattern Var)

--------------------------------------------------------------------------------

arithmetic :: Expr -> Expr
arithmetic = bottomUpExpr go
  where
  go (I32 0 :+ b) = b
  go (b :+ I32 0) = b
  go (I32 a :+ I32 b) = I32 (a + b)

  go (I32 0 :* _) = I32 0
  go (_ :* I32 0) = I32 0
  go (I32 1 :* b) = b
  go (a :* I32 1) = a
  go (I32 a :* I32 b) = I32 (a * b)

  go (Var (Intrinsic (DivI32 p)) :! _ :! I32 0) =
    case p of
      ZeroZDP      -> I32 0
      CrashZDP     -> error "NYI"
      UndefinedZDP -> Poison
  go (Var (Intrinsic (DivI32 p)) :! I32 0 :! _)
    | p `elem` [ZeroZDP, UndefinedZDP] = I32 0
  go (Var (Intrinsic (DivI32 _)) :! I32 a :! I32 b) = I32 (a `div` b)

  go (x1 :* y :+ x2 :* z) | x1 == x2 = x1 :* (y :+ z)
  go x = x
