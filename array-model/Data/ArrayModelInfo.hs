{-# LANGUAGE RecordWildCards #-}

module Data.ArrayModelInfo where

import qualified Data.Array.BitArray as B
import Data.ArrayModel (ArrayModel (..))
import Data.Ix (Ix)

data ArrayModelInfo w p = AMInfo
  { successors :: [(w, [w])],
    properties :: [(w, [p])]
  }

compile :: (Ix w, Ix p) => ArrayModelInfo w p -> ArrayModel w p
compile AMInfo {..} = AModel {..}
  where
    [w0, wn] =
      [minimum, maximum]
        <*> [ map fst successors
                <> map fst properties
                <> concatMap snd successors
            ]
    [p0, pn] = [minimum, maximum] <*> [concatMap snd properties]
    relation =
      B.false ((w0, w0), (wn, wn))
        B.// [((v, w), True) | (v, ws) <- successors, w <- ws]
    valuation =
      B.false ((w0, p0), (wn, pn))
        B.// [((w, p), True) | (w, ps) <- properties, p <- ps]
