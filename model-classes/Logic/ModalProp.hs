{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Logic.ModalProp where

import Algebra.Boolean (Boolean (..))
import Algebra.Frame (Frame (..))
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Logic.Model (Model (..))

data ModalProp p
  = Var p
  | Not (ModalProp p)
  | ModalProp p :/\: ModalProp p
  | ModalProp p :\/: ModalProp p
  | ModalProp p :->: ModalProp p
  | Box (ModalProp p)
  | Diamond (ModalProp p)
  deriving (Functor, Foldable, Traversable)

makeBaseFunctor ''ModalProp

eval :: (Frame m b, Model m p b) => m -> ModalProp p -> b
eval w f = cata step (fmap (w |=) f)
  where
    step = \case
      VarF b -> b
      NotF b -> complement b
      a :/\:$ b -> a .&. b
      a :\/:$ b -> a .|. b
      a :->:$ b -> a .>. b
      BoxF b -> box w b
      DiamondF b -> diamond w b
