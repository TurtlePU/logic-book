{-# LANGUAGE DeriveFunctor #-}

module Data.Result where

import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict (HashMap, (!?))
import Data.Hashable (Hashable)

data Result e a = Err e | Ok a deriving (Functor)

instance Bifunctor Result where
  bimap f _ (Err e) = Err (f e)
  bimap _ g (Ok a) = Ok (g a)

instance Monoid e => Applicative (Result e) where
  pure = Ok
  Err l <*> Err r = Err (l <> r)
  Err l <*> _ = Err l
  _ <*> Err r = Err r
  Ok l <*> Ok r = Ok (l r)

(^^^) :: (e -> e') -> Result e a -> Either e' a
f ^^^ (Err e) = Left (f e)
_ ^^^ (Ok a) = Right a

(??) :: Hashable k => HashMap k v -> k -> Result k v
m ?? k = maybe (Err k) Ok (m !? k)
