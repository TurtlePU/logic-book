{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.Frame where

import Algebra.Boolean (Boolean, complement)

class Boolean b => Frame m b where
  box :: m -> b -> b
  box m = complement . diamond m . complement

  diamond :: m -> b -> b
  diamond m = complement . box m . complement
