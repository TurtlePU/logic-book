{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.ArrayModel where

import Algebra.Boolean (Boolean (..))
import Algebra.Frame (Frame (..))
import Data.Array.BitArray (BitArray)
import qualified Data.Array.BitArray as B
import Data.Ix (Ix (range))
import Logic.ModalProp (ModalProp, eval)
import Logic.Model (Model (..))

data ArrayModel w p = AModel
  { relation :: BitArray (w, w),
    valuation :: BitArray (w, p)
  }

instance Ix w => Boolean (BitArray w) where
  complement = B.map not
  (.&.) = B.zipWith (&&)
  (.|.) = B.zipWith (||)
  (.>.) = B.zipWith (\x y -> not x || y)

instance Ix w => Frame (BitArray (w, w)) (BitArray w) where
  diamond mt vec =
    let ((x0, y0), (xn, yn)) = B.bounds mt
        row i = B.ixmap (y0, yn) (i,) mt
     in B.listArray (x0, xn) [B.or (row i .&. vec) | i <- range (x0, xn)]

instance Ix w => Frame (ArrayModel w p) (BitArray w) where
  diamond w t = relation w `diamond` t
  box w t = relation w `box` t

instance (Ix w, Ix p) => Model (ArrayModel w p) p (BitArray w) where
  m |= p =
    let ((w0, _), (wn, _)) = B.bounds (valuation m)
     in B.ixmap (w0, wn) (,p) (valuation m)

instance (Ix w, Ix p) => Model (ArrayModel w p, w) (ModalProp p) Bool where
  (m, w) |= p = eval m p B.! w
