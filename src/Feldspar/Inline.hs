module Feldspar.Inline
  ( -- * Inlining
    inline
  , inline'

    -- * Analysis
  , size
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), Expr, ExprF(..), pattern Let)
import Prelude
import Zabt (pattern Pat, subst1, pattern Var)

--------------------------------------------------------------------------------

inline :: Expr -> Expr
inline e = if e' == e then e' else inline e'
  where e' = inline' e

inline' :: Expr -> Expr
inline' (Var x) = Var x
inline' (Let x e1 e2) = do
  let e1' = inline' e1
  if size e1' <= 2
    then subst1 (x, e1') (inline' e2)
    else Let x e1' (inline' e2)
inline' (x :\ e) = x :\ inline' e
inline' (e1 :! e2) = inline' e1 :! inline' e2
inline' (Pat (Const c)) = Pat (Const c)

--------------------------------------------------------------------------------

size :: Expr -> Int
size (Var _) = 1
size (_ :\ e) = 1 + size e
size (e1 :! e2) = 1 + size e1 + size e2
size (Pat (Const _)) = 1
