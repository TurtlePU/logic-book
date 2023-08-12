{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ArrayModel where

import Algebra.Boolean (Boolean)
import Algebra.Frame (Frame (..))
import Data.Array.BitArray (BitArray)
import qualified Data.Array.BitArray as B
import Data.Ix (Ix)
import Logic.ModalProp (ModalProp)
import Logic.Model (Model (..))

data ArrayModel w p = AModel
  { relation :: BitArray (w, w),
    valuation :: BitArray (w, p)
  }

newtype ArrayTruthness w = Truthness {truthness :: BitArray w}
  deriving (Boolean)

instance Ix w => Frame (ArrayModel w p) (ArrayTruthness w) where
  diamond w t = Truthness (relation w `diamond` truthness t)
  box w t = Truthness (relation w `box` truthness t)

instance (Ix w, Ix p) => Model (ArrayModel w p) p (ArrayTruthness w) where
  m |= p =
    let ((w0, _), (wn, _)) = B.bounds (valuation m)
     in Truthness (B.ixmap (w0, wn) (,p) (valuation m))

instance (Ix w, Ix p) => Model (ArrayModel w p, w) (ModalProp p) Bool where
  (m, w) |= p = truthness (m |= p) B.! w
