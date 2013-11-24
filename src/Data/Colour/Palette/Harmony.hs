-----------------------------------------------------------------------------
-- |
-- Module      :  Palette.Harmony
-- Copyright   :  (c) 2013 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Utility functions to choose pleasing colors for creating color
-- schemes
--
-----------------------------------------------------------------------------

module Data.Colour.Palette.Harmony
       ( -- * Choosing color schemes

           Kolor

         -- ** Color utilities

         , tint, tone, shade, rotateColor

         -- ** Color harmonies

         , monochrome
         , complement
         , triad
         , tetrad
         , analogic
         , accentAnalogic
         , bwg

       ) where

import Data.Colour
import Data.Colour.SRGB         (RGB(..), toSRGB, sRGB)
import Data.Colour.RGBSpace.HSV
import Data.Colour.Names

type Kolor = Colour Double

-- This function and it's inverse below are the key to using the artists'
-- pigment color wheel. Red, blue and yellow are the primary colors and the
-- corresponding secondary colors are green, oragne and violet. We convert a
-- hue from the HSV in degrees (red = 0) to a hue on the artists' color wheel.
hsvToRyb :: Double -> Double
hsvToRyb x
  | x < 35    =       1.71 *  x
  | x < 60    = 60  + 2.40 * (x - 35)
  | x < 135   = 120 + 0.80 * (x - 60)
  | x < 225   = 180 + 0.67 * (x - 135)
  | x < 275   = 240 + 1.20 * (x - 225)
  | otherwise = 300 + 0.71 * (x - 275)

-- Convert of hue on the artists color wheel to a hue in HSV space.
rybToHsv :: Double -> Double
rybToHsv x
  | x < 60    =       0.58 *  x
  | x < 120   = 35  + 0.42 * (x - 60)
  | x < 180   = 60  + 1.25 * (x - 120)
  | x < 240   = 135 + 1.50 * (x - 180)
  | x < 300   = 225 + 0.83 * (x - 240)
  | otherwise = 275 + 1.42 * (x - 300)

rotateColor :: Double -> Kolor -> Kolor
rotateColor degrees c  = sRGB r g b
  where
    (h, s, v) = hsvView (toSRGB c)
    RGB r g b = hsv y s v
    k = (round $ hsvToRyb h + degrees :: Int) `mod` 360
    y = rybToHsv (fromIntegral k)

-- | Tints a color by adding blending t * white + (1-t) color.
--   t should be between 0 and 1.
tint :: Double -> Kolor -> Kolor
tint t c = blend t white c

-- | Alter the tone of a color by adding blending t * gray + (1-t) color.
--   t should be between 0 and 1.
tone :: Double -> Kolor -> Kolor
tone t c = blend t gray c

-- | Shades a color by adding blending s * black + (1-s) color.
--   t should be between 0 and 1.
shade :: Double -> Kolor -> Kolor
shade t c = blend t black c

-- | Create a monochromatic set of 5 colors based in the input color.
monochrome :: Kolor -> [Kolor]
monochrome c = [c, tint 0.25 c, tone 0.5 c, shade 0.5 c, shade 0.75 c]

-- | A color harmony using the base color and its opposite.
complement :: Kolor -> [Kolor]
complement c = [c, shade 0.5 d, tint 0.25 c, shade 0.75 c, d]
  where d = rotateColor 180 c

-- | A color chord based on three equally spaced hues.
triad :: Kolor -> [Kolor]
triad c = [ c, rotateColor 240 c, shade 0.5 $ rotateColor 210 c
          , shade 0.35 $ rotateColor 120 c, shade 0.67 c]

-- | Scheme based on 4 colors on a rectangle incscribed in the RYB color
--   wheel.
tetrad :: Kolor -> [Kolor]
tetrad c = [ c, rotateColor 180 c, tone 0.25 $ rotateColor 30 c
           , shade 0.5 $ rotateColor 210 c, tone 0.5 $ rotateColor 180 c]

-- | Chord base on three adjacent colors on the artists color wheel.
analogic :: Kolor -> [Kolor]
analogic c = [ c,  shade 0.3 $ rotateColor 330 c, tone 0.25 $ rotateColor 30 c
             , rotateColor 330 c, tone 0.5 c]

-- | Analogic chord plus the color opposite to the base color.
accentAnalogic :: Kolor -> [Kolor]
accentAnalogic c = [ c, tint 0.5 $ rotateColor 180 c
                   , tone 0.25 $ rotateColor 30 c, rotateColor 330 c
                   , rotateColor 180 c]

-- | Black, white and gray with a touch of the base color added.
bwg :: Kolor -> [Kolor]
bwg c = [c, tint 0.8 c, tone 0.8 c, shade 0.9 c]
