module Feldspar.Intrinsic
  ( Intrinsic(..)
  , ZeroDivisionPolicy(..)
  , prettyIntrinsic
  , prettyZeroDivisionPolicy
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude

data Intrinsic
  = AddI32
  | MulI32
  | DivI32 ZeroDivisionPolicy

deriving instance Show Intrinsic
deriving instance Eq Intrinsic
deriving instance Ord Intrinsic

data ZeroDivisionPolicy
  = ZeroZDP
  | CrashZDP
  | UndefinedZDP

deriving instance Show ZeroDivisionPolicy
deriving instance Eq ZeroDivisionPolicy
deriving instance Ord ZeroDivisionPolicy

prettyIntrinsic :: Intrinsic -> Text
prettyIntrinsic AddI32 = "#addI32"
prettyIntrinsic MulI32 = "#mulI32"
prettyIntrinsic (DivI32 p) = "#divI32$" <> prettyZeroDivisionPolicy p

prettyZeroDivisionPolicy :: ZeroDivisionPolicy -> Text
prettyZeroDivisionPolicy ZeroZDP      = "zero"
prettyZeroDivisionPolicy CrashZDP     = "crash"
prettyZeroDivisionPolicy UndefinedZDP = "undefined"
