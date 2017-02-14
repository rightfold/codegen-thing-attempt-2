module Feldspar.Inline
  ( -- * Inlining
    inline
  , inline'

    -- * Analysis
  , size
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), bottomUpExpr, Expr, ExprF(..), pattern Let)
import Prelude
import Zabt (pattern Pat, subst1, pattern Var)

--------------------------------------------------------------------------------

inline :: Expr -> Expr
inline e = if e' == e then e' else inline e'
  where e' = inline' e

inline' :: Expr -> Expr
inline' = bottomUpExpr go
  where
  go (Let x e1 e2)
    | size e1 <= 2 = subst1 (x, e1) e2
    | otherwise    = Let x e1 e2
  go x = x

--------------------------------------------------------------------------------

size :: Expr -> Int
size (Var _) = 1
size (_ :\ e) = 1 + size e
size (e1 :! e2) = 1 + size e1 + size e2
size (Pat (Const _)) = 1
size _ = error "NYI"
