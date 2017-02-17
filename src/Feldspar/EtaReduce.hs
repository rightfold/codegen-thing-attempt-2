module Feldspar.EtaReduce
  ( etaReduce
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), bottomUpExpr, Expr)
import Prelude
import Zabt (pattern Var)

etaReduce :: Expr -> Expr
etaReduce = bottomUpExpr go
  where
  go (x1 :\ (f :! Var x2)) | x1 == x2 = f
  go x = x
