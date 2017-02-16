module Feldspar.Intrinsic
  ( Intrinsic(..)
  , prettyIntrinsic
  ) where

import Data.Text (Text)
import Prelude

data Intrinsic
  = AddI32
  | MulI32

deriving instance Show Intrinsic
deriving instance Eq Intrinsic
deriving instance Ord Intrinsic

prettyIntrinsic :: Intrinsic -> Text
prettyIntrinsic AddI32 = "#addI32"
prettyIntrinsic MulI32 = "#mulI32"
