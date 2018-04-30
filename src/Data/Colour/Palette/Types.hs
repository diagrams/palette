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

-- | A convenient alias.
type Kolor = Colour Double

-- | Used to select the hue range in 'randomColor'.
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

-- | Used to select the luminosity range in 'randomColor'.
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
