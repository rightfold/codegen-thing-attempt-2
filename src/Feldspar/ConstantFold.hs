module Feldspar.ConstantFold
  ( constantFold
  ) where

import Feldspar.AST (pattern (:!), bottomUpExpr, Expr, ExprF(..), pattern I32)
import Feldspar.Intrinsics (pattern AddI32)
import Prelude
import Zabt (pattern Var)

--------------------------------------------------------------------------------

constantFold :: Expr -> Expr
constantFold = bottomUpExpr go
  where
  go (Var AddI32 :! I32 0 :! b) = b
  go (Var AddI32 :! a :! I32 0) = a
  go (Var AddI32 :! I32 a :! I32 b) = I32 (a + b)
  go x = x
