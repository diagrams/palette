-----------------------------------------------------------------------------
-- |
-- Module      :  Palette.Ty[es
-- Copyright   :  (c) 2013 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Types used in Palette modules
--
-----------------------------------------------------------------------------

module Data.Colour.Palette.Types
       (
         Kolor
       , Hue(..)
       , Luminosity(..)
       , ColorDefinition(..)

       ) where

import           Data.Colour

type Kolor = Colour Double

data Hue
  = HueMonochrome
  | HueRed
  | HueOrange
  | HueYellow
  | HueGreen
  | HueBlue
  | HuePurple
  | HuePink
  | HueRandom
  deriving (Show, Eq)

data Luminosity
  = LumBright
  | LumLight
  | LumDark
  | LumRandom
  deriving (Show, Eq)

data ColorDefinition = ColorDefinition
  { hueRange    :: Maybe (Int, Int)
  , lowerBounds :: [(Int, Int)]
  }
