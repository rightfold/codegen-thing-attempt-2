module Feldspar.ConstantFold
  ( constantFold
  ) where

import Feldspar.AST (pattern (:!), bottomUpExpr, Name(..), Expr, pattern I32)
import Feldspar.Intrinsic (Intrinsic(..))
import Prelude
import Zabt (pattern Var)

--------------------------------------------------------------------------------

constantFold :: Expr -> Expr
constantFold = bottomUpExpr go
  where
  go (Var (Intrinsic AddI32) :! I32 0 :! b) = b
  go (Var (Intrinsic AddI32) :! a :! I32 0) = a
  go (Var (Intrinsic AddI32) :! I32 a :! I32 b) = I32 (a + b)
  go x = x
