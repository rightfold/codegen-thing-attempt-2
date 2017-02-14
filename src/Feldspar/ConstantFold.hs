module Feldspar.ConstantFold
  ( constantFold
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), Expr, ExprF(..), pattern I32, pattern Let)
import Feldspar.Intrinsics (pattern AddI32)
import Prelude
import Zabt (pattern Pat, pattern Var)

--------------------------------------------------------------------------------

constantFold :: Expr -> Expr
constantFold (Var x) = Var x
constantFold (Let x e1 e2) =
  Let x (constantFold e1) (constantFold e2)
constantFold (x :\ e) = x :\ constantFold e
constantFold (Var AddI32 :! I32 a :! I32 b) = I32 (a + b)
constantFold (e1 :! e2) = constantFold e1 :! constantFold e2
constantFold (Pat (Const c)) = Pat (Const c)
constantFold _ = error "NYI"
