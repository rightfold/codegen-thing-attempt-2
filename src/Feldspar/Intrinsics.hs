module Feldspar.Intrinsics where

import Feldspar.AST (Name(Global))

pattern AddI32 :: Name
pattern AddI32 = Global 0 "$intrinsic$addI32"
