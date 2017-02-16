module Feldspar.Inline
  ( Inline(..)
  , inline
  , inline'
  ) where

import Feldspar.AST (bottomUpExpr, Expr, pattern Let, sizeExpr)
import Prelude
import Zabt (subst1)

data Inline = Inline
  { inlineThreshold :: Int
  }

inline :: Inline -> Expr -> Expr
inline i e = if e' == e then e' else inline i e'
  where e' = inline' i e

inline' :: Inline -> Expr -> Expr
inline' i = bottomUpExpr go
  where
  go (Let x e1 e2)
    | sizeExpr e1 <= inlineThreshold i = subst1 (x, e1) e2
    | otherwise                        = Let x e1 e2
  go x = x
