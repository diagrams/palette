-----------------------------------------------------------------------------
-- |
-- Module      :  Palette.Harmony
-- Copyright   :  (c) 2013 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Utility functions to creating color schemes.
--
-----------------------------------------------------------------------------

module Data.Colour.Palette.Harmony
       ( -- * Choosing color schemes

        -- ** Synonym for Colour Double

           Kolor

         -- ** Color utilities

         , tint, tone, shade, sliders, rotateColor

         -- ** Color harmonies

         , monochrome
         , complement
         , triad
         , tetrad
         , analogic
         , accentAnalogic
         , bwg
         , colorRamp
         , randomColor
         , randomHarmony
         , randomPalette


       ) where

import           Control.Monad.Random
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.Palette.Types
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.SRGB          (RGB (..), sRGB, sRGB24read, toSRGB)
import           Text.Printf

-- > import Data.Colour.Palette.Harmony
-- > import Data.Colour.SRGB (sRGB24read)
-- > wheel [] = circle 1 # fc black
-- > wheel cs = wheel' # rotateBy r
-- >   where
-- >     wheel' = mconcat $ zipWith fc cs (iterateN n (rotateBy a) w)
-- >     n = length cs
-- >     a = 1 / (fromIntegral n) :: Turn
-- >     w = wedge 1 (0 :: Turn) a # lw 0
-- >     r = 1/4 - 1/(2*(fromIntegral n))
-- > base = sRGB24read "#95a78d"
-- > mono = wheel $ monochrome base
-- > comp = wheel $ complement base
-- > tria = wheel $ triad base
-- > tetr = wheel $ tetrad base
-- > anal = wheel $ analogic base
-- > acce = wheel $ accentAnalogic base
-- > bw = wheel $ bwg base

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

-- Rotate a hue on the RYB color wheel.
rotateHue :: Double -> Double -> Double
rotateHue h degrees =  rybToHsv (fromIntegral k)
  where
    k = (round $ hsvToRyb h + degrees :: Int) `mod` 360

-- | Rotate a color and apply one function to its saturation and another to its value.
sliders :: Kolor -> Double -> (Double -> Double)
       -> (Double -> Double) -> Kolor
sliders c rot fs fv = sRGB r g b
  where
    (h, s, v) = hsvView (toSRGB c)
    h' = rotateHue h rot
    s' = max 0 (min 1 (fs s))
    v' = max 0 (min 1 (fv v))
    RGB r g b = hsv h' s' v'

-- | Rotate a color on the RYB color wheel
rotateColor :: Double -> Kolor -> Kolor
rotateColor degrees c  = sRGB r g b
  where
    (h, s, v) = hsvView (toSRGB c)
    RGB r g b = hsv (rotateHue h degrees) s v

-- | Tints a color by adding blending t * white + (1 - t) color.
--   t should be between 0 and 1.
tint :: Double -> Kolor -> Kolor
tint t = blend t white

-- | Alter the tone of a color by adding blending t * gray + (1 - t) color.
--   t should be between 0 and 1.
tone :: Double -> Kolor -> Kolor
tone t = blend t gray

-- | Shades a color by adding blending s * black + (1 - t) color.
--   t should be between 0 and 1.
shade :: Double -> Kolor -> Kolor
shade t = blend t black

-- | Create a monochromatic set of 5 colors based in the input color.
-- <<diagrams/src_Data_Colour_Palette_Harmony_mono.svg#diagram=mono&width=200>>
monochrome :: Kolor -> [Kolor]
monochrome c = [c, tint 0.25 c, tone 0.5 c, shade 0.5 c, shade 0.75 c]

-- | A color harmony using the base color and its opposite.
-- <<diagrams/src_Data_Colour_Palette_Harmony_comp.svg#diagram=comp&width=200>>
complement :: Kolor -> [Kolor]
complement c = [c, shade 0.5 d, tint 0.25 c, shade 0.75 c, d]
  where d = rotateColor 180 c

-- | A color chord based on three equally spaced hues.
-- <<diagrams/src_Data_Colour_Palette_Harmony_tria.svg#diagram=tria&width=200>>
triad :: Kolor -> [Kolor]
triad c = [ c, rotateColor 240 c, shade 0.5 $ rotateColor 210 c
          , shade 0.35 $ rotateColor 120 c, shade 0.67 c]

-- | Scheme based on 4 colors on a rectangle incscribed in the RYB color
--   wheel.
-- <<diagrams/src_Data_Colour_Palette_Harmony_tetr.svg#diagram=tetr&width=200>>
tetrad :: Kolor -> [Kolor]
tetrad c = [ c, rotateColor 180 c, tone 0.25 $ rotateColor 30 c
           , shade 0.5 $ rotateColor 210 c, tone 0.5 $ rotateColor 180 c]

-- | Chord base on three adjacent colors on the artists color wheel.
-- <<diagrams/src_Data_Colour_Palette_Harmony_anal.svg#diagram=anal&width=200>>
analogic :: Kolor -> [Kolor]
analogic c = [ c,  shade 0.3 $ rotateColor 330 c, tone 0.25 $ rotateColor 30 c
             , rotateColor 330 c, tone 0.5 c]

-- | Analogic chord plus the color opposite to the base color.
-- <<diagrams/src_Data_Colour_Palette_Harmony_acce.svg#diagram=acce&width=200>>
accentAnalogic :: Kolor -> [Kolor]
accentAnalogic c = [ c, tint 0.5 $ rotateColor 180 c
                   , tone 0.25 $ rotateColor 30 c, rotateColor 330 c
                   , rotateColor 180 c]

-- | Black, white and gray with a touch of the base color added.
-- <<diagrams/src_Data_Colour_Palette_Harmony_bw.svg#diagram=bw&width=200>>
bwg :: Kolor -> [Kolor]
bwg c = [c, tint 0.8 c, tone 0.8 c, shade 0.9 c]

-- | Interpolate n colors from a list of colors using linear piecewise
-- interpolation to add additional colors to a palette.
colorRamp :: Int -> [Kolor] -> [Kolor]
colorRamp n xs0 = if n <= length xs0 then take n xs0 else take n (go 0 xs0)
  where
    di = fromIntegral (length xs0 - 1) / fromIntegral (n - 1)
    go _ [x] = [x]
    go i xs'@(x1 : xs@(x2 : _))
        | i > 1 = go (i - 1) xs
        | otherwise = blend i x2 x1 : go (i + di) xs'
    go _ _ = []

-- | Generate a random opaque color
randomColor :: MonadRandom m => m Kolor
randomColor = do
  x <- getRandomR (0.1 :: Double, 0.9)
  let n = floor $ x * 167777215 :: Int
      hex = printf "#%06X" n
  return $ sRGB24read hex

-- | return a random harmony based on a seed color.
randomHarmony :: MonadRandom m => Kolor -> m [Kolor]
randomHarmony c = do
  harmony <- uniform
             [ monochrome
             , complement
             , triad
             , tetrad
             , analogic
             , accentAnalogic
             ]
  return $ harmony c

-- | Generate a random color palette.
randomPalette :: MonadRandom m => m [Kolor]
randomPalette = randomColor >>= randomHarmony
