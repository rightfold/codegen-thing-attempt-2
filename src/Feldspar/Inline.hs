module Feldspar.Inline
  ( -- * Inlining
    Inline(..)
  , inline
  , inline'

    -- * Analysis
  , size
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), bottomUpExpr, Expr, ExprF(..), pattern Let)
import Prelude
import Zabt (pattern Pat, subst1, pattern Var)

--------------------------------------------------------------------------------

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
    | size e1 <= inlineThreshold i = subst1 (x, e1) e2
    | otherwise                    = Let x e1 e2
  go x = x

--------------------------------------------------------------------------------

size :: Expr -> Int
size (Var _) = 1
size (_ :\ e) = 1 + size e
size (e1 :! e2) = 1 + size e1 + size e2
size (Pat (Const _)) = 1
size _ = error "NYI"
