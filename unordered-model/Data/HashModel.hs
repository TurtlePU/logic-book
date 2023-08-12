{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

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

data WorldInfo w p = WInfo
  { successors :: HashSet w,
    properties :: HashSet p
  }

data ModelInfo w p = MInfo
  { knownProps :: HashSet p,
    knownWorlds :: HashMap w (WorldInfo w p)
  }

data MappingError w p
  = UnknownWorlds (HashSet w)
  | UnknownProps (HashSet p)

compile ::
  (Hashable w, Hashable p) =>
  ModelInfo w p ->
  Either (MappingError w p) (HashModel w p)
compile info = do
  let worlds = compress $ keysSet (knownWorlds info)
      props = compress (knownProps info)
      knownWorlds' = mapKeys (worlds !) (knownWorlds info)
      sucRes =
        for (successors <$> knownWorlds') . traverseSet $
          first singleton . (worlds ??)
      propRes =
        for (properties <$> knownWorlds') . traverseSet $
          first singleton . (props ??)
  sucMap <- UnknownWorlds ^^^ sucRes
  propMap <- UnknownProps ^^^ propRes
  let inner =
        AModel
          { relation = _,
            valuation = _
          }
  pure HModel {..}
  where
    compress :: Hashable a => HashSet a -> HashMap a Int
    compress = fromList . flip zip [0 ..] . toList

    traverseSet :: (Hashable a, Hashable b, Applicative f) => (a -> f b) -> HashSet a -> f (HashSet b)
    traverseSet action = foldl' (\s a -> liftA2 insert (action a) s) (pure empty)

instance
  (Hashable w, Hashable p) =>
  Model (HashModel w p, w) (ModalProp p) (Either (MappingError w p) Bool)
  where
  (m, w) |= p = do
    iw <- (UnknownWorlds . singleton) ^^^ (worlds m ?? w)
    ip <- UnknownProps ^^^ traverse (first singleton . (props m ??)) p
    pure $ (inner m, iw) |= ip
