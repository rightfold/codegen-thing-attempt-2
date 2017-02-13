module Feldspar.Inline
  ( -- * Inlining
    inline

    -- * Analysis
  , size
  ) where

import Data.Maybe (fromMaybe)
import Feldspar.AST (pattern (:\), pattern (:!), Expr, pattern Let, Name)
import Prelude
import Zabt (subst1, pattern Var)

--------------------------------------------------------------------------------

inline :: Expr -> Expr
inline (Var x) = Var x
inline (Let x e1 e2) = do
  let e1' = inline e1
  if size e1' <= 2
    then Let x e1' (subst1 (x, e1') e2)
    else Let x e1' (inline e2)
inline (x :\ e) = x :\ inline e
inline (e1 :! e2) = inline e1 :! inline e2

--------------------------------------------------------------------------------

size :: Expr -> Int
size (Var _) = 1
size (_ :\ e) = 1 + size e
size (e1 :! e2) = 1 + size e1 + size e2
