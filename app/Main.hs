{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forever)
import Data.HashModel (MappingError)
import Data.HashModelInfo (ModelInfo (..), compile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Logic.Model ((|=))
import Parser (parseProp)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  knownProps <- T.splitOn " " <$> TIO.getLine
  knownWorlds <- getWorlds
  case compile MInfo {..} of
    Left err -> print err
    Right model -> forever $ do
      (world, rest) <- T.breakOn " " <$> TIO.getLine
      case parseProp "<stdin>" (T.strip rest) of
        Left err -> putStrLn (errorBundlePretty err)
        Right formula -> print ((model, world) |= formula)
  where
    getWorlds :: IO [(T.Text, [T.Text], [T.Text])]
    getWorlds = do
      w <- TIO.getLine
      if w == T.empty
        then pure []
        else do
          ws <- T.splitOn " " <$> TIO.getLine
          let ws' = if ws == [T.empty] then [] else ws
          ps <- T.splitOn " " <$> TIO.getLine
          let ps' = if ps == [T.empty] then [] else ps
          ((w, ws', ps') :) <$> getWorlds
