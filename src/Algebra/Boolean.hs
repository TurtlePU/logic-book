module Algebra.Boolean where

import Data.Array.BitArray (BitArray)
import qualified Data.Array.BitArray as B
import Data.Ix (Ix)

class Boolean b where
  complement :: b -> b

  infixl 6 .&.
  (.&.) :: b -> b -> b
  x .&. y = complement (complement x .|. complement y)

  infixl 5 .|.
  (.|.) :: b -> b -> b
  x .|. y = complement (complement x .&. complement y)

  infixr 4 .>.
  (.>.) :: b -> b -> b
  x .>. y = complement x .|. y

instance Ix i => Boolean (BitArray i) where
  complement = B.map not
  (.&.) = B.zipWith (&&)
  (.|.) = B.zipWith (||)
  (.>.) = B.zipWith (\x y -> not x || y)
