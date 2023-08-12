{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.HashModel where

import Control.Applicative (Applicative (liftA2))
import Data.ArrayModel (ArrayModel (AModel, relation, valuation))
import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap, fromList, keysSet, mapKeys, (!))
import Data.HashSet (HashSet, empty, foldl', insert, singleton, toList)
import Data.Hashable (Hashable)
import Data.Result ((??), (^^^))
import Data.Traversable (for)
import Logic.ModalProp (ModalProp)
import Logic.Model (Model (..))

data HashModel w p = HModel
  { inner :: ArrayModel Int Int,
    worlds :: HashMap w Int,
    props :: HashMap p Int
  }

data MappingError w p
  = UnknownWorlds (HashSet w)
  | UnknownProps (HashSet p)
  deriving (Show)

instance
  (Hashable w, Hashable p) =>
  Model (HashModel w p, w) (ModalProp p) (Either (MappingError w p) Bool)
  where
  (m, w) |= p = do
    iw <- UnknownWorlds . singleton ^^^ worlds m ?? w
    ip <- UnknownProps ^^^ traverse (first singleton . (props m ??)) p
    pure $ (inner m, iw) |= ip
