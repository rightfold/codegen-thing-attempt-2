module Feldspar.EtaReduce
  ( etaReduce
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), bottomUpExpr, Expr)
import Prelude
import Zabt (freeVars, pattern Var)

import qualified Data.Set as Set

etaReduce :: Expr -> Expr
etaReduce = bottomUpExpr go
  where
  go (x1 :\ (f :! Var x2)) | x1 == x2 && not (x1 `Set.member` freeVars f) = f
  go x = x
