{-# LANGUAGE RecordWildCards #-}

module Data.HashModelInfo where

import Control.Applicative (liftA2)
import Data.ArrayModel (ArrayModel)
import qualified Data.ArrayModelInfo as A
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as M
import Data.HashModel (HashModel (..), MappingError (..))
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Result ((??), (^^^))
import Data.Traversable (for)

data ModelInfo w p = MInfo
  { knownProps :: [p],
    knownWorlds :: [(w, [w], [p])]
  }

compile ::
  (Hashable w, Hashable p) =>
  ModelInfo w p ->
  Either (MappingError w p) (HashModel w p)
compile info = do
  let props = compress (knownProps info)
      worlds = compress [w | (w, _, _) <- knownWorlds info]
      remapped =
        M.fromList [(worlds M.! w, (ws, ps)) | (w, ws, ps) <- knownWorlds info]
  successors <-
    UnknownWorlds ^^^ fmap M.toList $
      for (fst <$> remapped) $
        traverse $
          first S.singleton . (worlds ??)
  properties <-
    UnknownProps ^^^ fmap M.toList $
      for (snd <$> remapped) $
        traverse $
          first S.singleton . (props ??)
  let inner = A.compile A.AMInfo {..} :: ArrayModel Int Int
  pure HModel {..}
  where
    compress :: Hashable a => [a] -> M.HashMap a Int
    compress = M.fromList . flip zip [0 ..] . S.toList . S.fromList
