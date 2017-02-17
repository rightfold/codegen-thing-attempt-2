module Language.ECMAScript.Pretty
  ( prettyJSProgram
  , prettyJSExpr
  , prettyJSStmt
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Language.ECMAScript.AST (JSExpr(..), JSProgram(..), JSStmt(..))
import Prelude

import qualified Data.Text as Text

prettyJSProgram :: JSProgram -> Text
prettyJSProgram (JSProgram ss) = foldMap prettyJSStmt ss

prettyJSExpr :: JSExpr -> Text
prettyJSExpr (JSIdent x) = x
prettyJSExpr (JSStaticMember o p) = "(" <> prettyJSExpr o <> "." <> p <> ")"
prettyJSExpr (JSFunction ps ss) =
  "(function(" <> Text.intercalate ", " ps <> ") {\n"
  <> foldMap prettyJSStmt ss
  <> "})"
prettyJSExpr (JSCall f xs) =
  "(" <> prettyJSExpr f
  <> "(" <> Text.intercalate ", " (map prettyJSExpr xs) <> ")"
  <> ")"
prettyJSExpr (JSNumber n) = Text.pack (show n)

prettyJSStmt :: JSStmt -> Text
prettyJSStmt (JSExpr e) = prettyJSExpr e <> ";\n"
prettyJSStmt (JSConst x e) = "const " <> x <> " = " <> prettyJSExpr e <> ";\n"
prettyJSStmt (JSReturn Nothing) = "return;\n"
prettyJSStmt (JSReturn (Just e)) = "return " <> prettyJSExpr e <> ";\n"
