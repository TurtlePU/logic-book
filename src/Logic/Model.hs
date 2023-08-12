{-# LANGUAGE MultiParamTypeClasses #-}

module Logic.Model where

class Model m p b where
  infix 3 |=
  (|=) :: m -> p -> b
