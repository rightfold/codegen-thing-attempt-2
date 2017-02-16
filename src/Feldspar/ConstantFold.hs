module Feldspar.ConstantFold
  ( constantFold
  ) where

import Feldspar.AST (pattern (:+), pattern (:*), bottomUpExpr, Expr, pattern I32)
import Prelude

--------------------------------------------------------------------------------

constantFold :: Expr -> Expr
constantFold = bottomUpExpr go
  where
  go (I32 0 :+ b) = b
  go (b :+ I32 0) = b
  go (I32 a :+ I32 b) = I32 (a + b)

  go (I32 0 :* _) = I32 0
  go (_ :* I32 0) = I32 0
  go (I32 1 :* b) = b
  go (a :* I32 1) = a
  go (I32 a :* I32 b) = I32 (a * b)

  go (x1 :* y :+ x2 :* z) | x1 == x2 = x1 :* (y :+ z)
  go x = x
