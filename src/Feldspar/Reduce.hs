module Feldspar.Reduce
  ( reduce
  , reduce'
  ) where

import Feldspar.AST (pattern (:\), pattern (:$), Expr)
import Prelude
import Zabt (subst1, pattern Var)

reduce :: Int -> Expr -> Expr
reduce n e
  | n <= 0    = e
  | otherwise = reduce (n - 1) (reduce' e)

reduce' :: Expr -> Expr
reduce' x@(Var _) = x
reduce' x@(_ :\ _) = x
reduce' ((v :\ e) :$ x) = subst1 (v, x) e
