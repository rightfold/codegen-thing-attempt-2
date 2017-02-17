module Language.ECMAScript.AST
  ( JSProgram(..)
  , JSExpr(..)
  , JSStmt(..)
  ) where

import Data.Text (Text)
import Prelude

newtype JSProgram = JSProgram [JSStmt]

data JSExpr
  = JSIdent Text
  | JSStaticMember JSExpr Text
  | JSFunction [Text] [JSStmt]
  | JSCall JSExpr [JSExpr]
  | JSNumber Double

data JSStmt
  = JSExpr JSExpr
  | JSConst Text JSExpr
  | JSReturn (Maybe JSExpr)
