module Feldspar.Intrinsic
  ( Intrinsic(..)
  , prettyIntrinsic
  ) where

import Data.Text (Text)
import Prelude

data Intrinsic
  = AddI32

deriving instance Eq Intrinsic
deriving instance Ord Intrinsic

prettyIntrinsic :: Intrinsic -> Text
prettyIntrinsic AddI32 = "#addI32"
