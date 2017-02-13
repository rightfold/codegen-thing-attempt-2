module Feldspar.AST
  ( -- * Names
    Name(..)

    -- * Expressions
  , ExprF(..)
  , Expr
  , pattern (:\)
  , pattern (:!)
  , pattern Let

    -- * Pretty
  , prettyName
  , prettyExpr
  ) where

import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)
import Prelude
import Zabt (pattern Abs, Freshen(..), pattern Pat, Term, pattern Var)

import qualified Data.Text as Text

--------------------------------------------------------------------------------

infixr 0 :\
infixl 1 :!

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
  | Lam a

deriving instance (Eq a) => Eq (ExprF a)
deriving instance Functor ExprF
deriving instance Foldable ExprF

type Expr = Term Name ExprF

pattern (:\) :: Name -> Expr -> Expr
pattern (:\) x e = Pat (Lam (Abs x e))

pattern (:!) :: Expr -> Expr -> Expr
pattern (:!) e1 e2 = Pat (App e1 e2)

pattern Let :: Name -> Expr -> Expr -> Expr
pattern Let x e1 e2 = (x :\ e2) :! e1

--------------------------------------------------------------------------------

prettyName :: Name -> Text
prettyName (Name 0 t) = t
prettyName (Name i t) = t <> "$" <> Text.pack (show i)

prettyExpr :: Expr -> Text
prettyExpr (Var x) = prettyName x
prettyExpr (Let x e1 e2) =
  "(let " <> prettyName x <> " = "
  <> prettyExpr e1 <> " in "
  <> prettyExpr e2 <> ")"
prettyExpr (x :\ e) =
  "(fun " <> prettyName x <> " -> "
  <> prettyExpr e <> ")"
prettyExpr (e1 :! e2) =
  "(" <> prettyExpr e1 <> " "
  <> prettyExpr e2 <> ")"
