module Feldspar.AST
  ( -- * Names
    Name(..)
    -- * Expressions
  , ExprF(..)
  , Expr
  , pattern (:\)
  , pattern (:$)

    -- * Types
  , Type(..)
  ) where

import Data.String (IsString(..))
import Data.Text (Text)
import Prelude
import Zabt (pattern Abs, Freshen(..), pattern Pat, Term)

--------------------------------------------------------------------------------

infixr 0 :\
infixr 1 :$

--------------------------------------------------------------------------------

data Name = Name Int Text

deriving instance Eq Name
deriving instance Ord Name

instance IsString Name where
  fromString = Name 0 . fromString

instance Freshen Name where
  freshen (Name n t) = Name (n + 1) t

--------------------------------------------------------------------------------

data ExprF a
  = App a a
  | Lam Type a

deriving instance (Eq a) => Eq (ExprF a)
deriving instance Functor ExprF
deriving instance Foldable ExprF

type Expr = Term Name ExprF

pattern (:\) :: Name -> Expr -> Expr
pattern (:\) x e = Pat (Lam Number (Abs x e))

pattern (:$) :: Expr -> Expr -> Expr
pattern (:$) e1 e2 = Pat (App e1 e2)

--------------------------------------------------------------------------------

data Type
  = Number
  | Boolean
  | Function Type Type

deriving instance Eq Type
