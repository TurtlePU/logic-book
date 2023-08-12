{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Algebra.Frame where

import Algebra.Boolean (Boolean, complement, (.&.))
import Data.Array.BitArray (BitArray)
import qualified Data.Array.BitArray as B
import Data.Ix (Ix, range)

class Boolean b => Frame m b where
  box :: m -> b -> b
  box m = complement . diamond m . complement

  diamond :: m -> b -> b
  diamond m = complement . box m . complement

instance Ix i => Frame (BitArray (i, i)) (BitArray i) where
  diamond mt vec =
    let ((x0, y0), (xn, yn)) = B.bounds mt
        row i = B.ixmap (y0, yn) (i,) mt
     in B.listArray (x0, xn) [B.or (row i .&. vec) | i <- range (x0, xn)]
