module Feldspar.AST
  ( -- * Names
    Name(..)
  , isLocal

    -- * Expressions
  , ExprF(..)
  , Expr
  , pattern (:\)
  , pattern (:!)
  , pattern (:+)
  , pattern (:*)
  , pattern Let
  , pattern I32
  , bottomUpExpr
  , bottomUpExprM
  , bottomUpExprChildren
  , bottomUpExprChildrenM

    -- * Constants
  , Const(..)

    -- * Pretty
  , prettyName
  , prettyExpr
  , prettyConst

    -- * Analysis
  , sizeExpr
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)
import Feldspar.Intrinsic (Intrinsic(..), prettyIntrinsic)
import Prelude
import Zabt (pattern Abs, Freshen(..), pattern Pat, Term, pattern Var)

import qualified Data.Text as Text

--------------------------------------------------------------------------------

infixr 0 :\
infixl 1 :!
infixl 6 :+
infixl 7 :*

--------------------------------------------------------------------------------

data Name
  = Local Int Text
  | Global Text
  | Intrinsic Intrinsic

deriving instance Show Name
deriving instance Eq Name
deriving instance Ord Name

instance IsString Name where
  fromString = Local 0 . fromString

instance Freshen Name where
  freshen (Local n t) = Local (n + 1) t
  freshen (Global t) = Global t
  freshen (Intrinsic i) = Intrinsic i

isLocal :: Name -> Bool
isLocal (Local _ _) = True
isLocal _ = False

--------------------------------------------------------------------------------

data ExprF a
  = App a a
  | Lam a
  | Const Const

deriving instance (Show a) => Show (ExprF a)
deriving instance (Eq a) => Eq (ExprF a)
deriving instance Functor ExprF
deriving instance Foldable ExprF

type Expr = Term Name ExprF

pattern (:\) :: Name -> Expr -> Expr
pattern (:\) x e = Pat (Lam (Abs x e))

pattern (:!) :: Expr -> Expr -> Expr
pattern (:!) e1 e2 = Pat (App e1 e2)

pattern (:+) :: Expr -> Expr -> Expr
pattern (:+) e1 e2 = Var (Intrinsic AddI32) :! e1 :! e2

pattern (:*) :: Expr -> Expr -> Expr
pattern (:*) e1 e2 = Var (Intrinsic MulI32) :! e1 :! e2

pattern Let :: Name -> Expr -> Expr -> Expr
pattern Let x e1 e2 = (x :\ e2) :! e1

pattern I32 :: Int32 -> Expr
pattern I32 n = Pat (Const (I32Const n))

bottomUpExpr :: (Expr -> Expr) -> Expr -> Expr
bottomUpExpr f = runIdentity . bottomUpExprM (Identity . f)

bottomUpExprM :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
bottomUpExprM k e = k =<< bottomUpExprChildrenM k e

bottomUpExprChildren :: (Expr -> Expr) -> Expr -> Expr
bottomUpExprChildren f = runIdentity . bottomUpExprChildrenM (Identity . f)

bottomUpExprChildrenM :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
bottomUpExprChildrenM _ (Var x) = pure (Var x)
bottomUpExprChildrenM k (x :\ e) = (x :\) <$> bottomUpExprM k e
bottomUpExprChildrenM k (e1 :! e2) = (:!) <$> bottomUpExprM k e1 <*> bottomUpExprM k e2
bottomUpExprChildrenM _ (Pat (Const c)) = pure (Pat (Const c))
bottomUpExprChildrenM _ _ = error "NYI"

--------------------------------------------------------------------------------

data Const
  = I32Const Int32

deriving instance Show Const
deriving instance Eq Const
deriving instance Ord Const

--------------------------------------------------------------------------------

prettyName :: Name -> Text
prettyName (Local 0 t) = "%" <> t
prettyName (Local i t) = "%" <> t <> "$" <> Text.pack (show i)
prettyName (Global t) = "@" <> t
prettyName (Intrinsic i) = prettyIntrinsic i

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
prettyExpr (Pat (Const c)) = prettyConst c
prettyExpr _ = error "NYI"

prettyConst :: Const -> Text
prettyConst (I32Const i) = Text.pack (show i)

--------------------------------------------------------------------------------

sizeExpr :: Expr -> Int
sizeExpr (Var _) = 1
sizeExpr (_ :\ e) = 1 + sizeExpr e
sizeExpr (e1 :! e2) = 1 + sizeExpr e1 + sizeExpr e2
sizeExpr (Pat (Const _)) = 1
sizeExpr _ = error "NYI"
