module Algebra.Boolean where

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
