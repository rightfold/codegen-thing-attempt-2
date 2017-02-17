module Feldspar.ECMAScript
  ( nameToExpr
  , exprToExpr
  , exprToStmts
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Feldspar.AST (pattern(:\), pattern(:!), Const(..), Expr, ExprF(..), pattern Let, Name(..))
import Feldspar.Intrinsic (Intrinsic(..))
import Language.ECMAScript.AST (JSExpr(..), JSStmt(..))
import Prelude
import Zabt (pattern Pat, pattern Var)

import qualified Data.Text as Text

localNameToIdent :: Int -> Text -> Text
localNameToIdent 0 x = x
localNameToIdent i x = x <> "$" <> Text.pack (show i)

nameToExpr :: Name -> JSExpr
nameToExpr (Local i x) = JSIdent (localNameToIdent i x)
nameToExpr (Global n) = JSStaticMember (JSIdent "__global") n
nameToExpr (Intrinsic AddI32) = JSIdent "__addI32"
nameToExpr (Intrinsic MulI32) = JSIdent "__mulI32"

exprToExpr :: Expr -> JSExpr
exprToExpr (Var x) = nameToExpr x
exprToExpr (Local i x :\ e) =
  JSFunction [localNameToIdent i x]
             (exprToStmts (pure . JSReturn . Just) e)
exprToExpr (e1 :! e2) = JSCall (exprToExpr e1) [exprToExpr e2]
exprToExpr (Pat (Const c)) = constToExpr c
exprToExpr _ = error "NYI"

exprToStmts :: (JSExpr -> [JSStmt]) -> Expr -> [JSStmt]
exprToStmts f (Let (Local i x) e1 e2) =
  exprToStmts (pure . JSConst (localNameToIdent i x)) e1
  <> exprToStmts f e2
exprToStmts f e = f $ exprToExpr e

constToExpr :: Const -> JSExpr
constToExpr (I32Const c) = JSNumber (fromIntegral c)
