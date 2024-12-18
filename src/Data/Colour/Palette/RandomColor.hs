{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Palette.RandomColor
-- Copyright   :  (c) 2018 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Functions to create random colors.
-- Includes a port of [David Merfield's](https://github.com/davidmerfield/randomColor)
-- randomColor.
-----------------------------------------------------------------------------

module Data.Colour.Palette.RandomColor
  ( -- * A Library for generating random colors.

    -- ** Choose a random color from CIELAB colorspace.
    randomCIELab
  , randomCIELabPalette

    -- ** Choose a random color using David Merfield's algorithm
  , randomColor
  , randomPalette
  , randomHarmony

  -- ** Choose a random HSV component

  , randomHue
  , randomSaturation
  , randomBrightness

  ) where

import           Control.Monad.Random
import           Data.Colour.CIE            (cieLAB)
import           Data.Colour.CIE.Illuminant (d65)
import           Data.Colour.Palette.Harmony
import           Data.Colour.Palette.Types
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.SRGB            (RGB (..), sRGB)
import           Data.List                   (find)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe                  (fromMaybe)

getColorDefinition :: Hue -> ColorDefinition
getColorDefinition = \case
  HueMonochrome -> ColorDefinition Nothing [(0,0),(100, 0)]
  HueRed        -> ColorDefinition (Just (-26,18))
                     [ (20,100), (30,92), (40,89), (50,85), (60,78), (70,70)
                     , (80,60), (90,55), (100,50)
                     ]
  HueOrange     -> ColorDefinition (Just (19,46))
                     [ (20,100), (30,93), (40,88), (50,86), (60,85), (70,70)
                     , (100,70)
                     ]
  HueYellow     -> ColorDefinition (Just (47,62))
                     [ (25,100), (40,94), (50,89), (60,86), (70,84), (80,82)
                     , (90,80), (100,75)
                     ]
  HueGreen      -> ColorDefinition (Just (63,178))
                     [ (30,100), (40,90), (50,85), (60,81), (70,74), (80,64)
                     , (90,50), (100,40)
                     ]
  HueBlue       -> ColorDefinition (Just (179,257))
                     [ (20,100), (30,86), (40,80), (50,74), (60,60), (70,52)
                     , (80,44), (90,39), (100,35)
                     ]
  HuePurple     -> ColorDefinition (Just (258,282))
                     [ (20,100), (30,87), (40,79), (50,70), (60,65), (70,59)
                     , (80,52), (90,45), (100,42)
                     ]
  HuePink       -> ColorDefinition (Just (283,334))
                     [ (20,100), (30,90), (40,86), (60,84), (80,80), (90,75)
                     , (100,73)
                     ]
  HueRandom     -> ColorDefinition (Just (0, 359)) []

getHue :: Int -> Hue
getHue n
  | n' == 0   = HueMonochrome
  | n' >= 283 = HuePink
  | n' >= 258 = HuePurple
  | n' >= 179 = HueBlue
  | n' >= 63  = HueGreen
  | n' >= 47  = HueYellow
  | n' >= 19  = HueOrange
  | n' >= -26 = HueRed
  | otherwise = error "getHue: hue outside [0, 360]"
  where
    n' = if n >= 334 && n <= 360 then n - 360 else n

-- | Return a random hue in the range $[lo, hi]$ as a 'Double'.
--   lo should be >= 0 and hi < 360.
--   Instead of storing red as two seperate ranges we create a single
--   contiguous range using negative numbers.
randomHue :: MonadRandom m => Hue -> m Int
randomHue h = do
  hue <- getRandomR (lo, hi)
  return $ if hue < 0 then hue + 360 else hue
  where
    hr = hueRange . getColorDefinition $ h
    (lo, hi) = fromMaybe (0, 0) hr

saturationRange :: Hue -> (Int, Int)
saturationRange hue = result
  where
    lbs = lowerBounds $ getColorDefinition hue
    result = case NE.nonEmpty lbs of
      Nothing -> error "Can\'t obtain saturationRange from an empty lowerBounds"
      Just lbsNE -> (fst . NE.head $ lbsNE, fst . NE.last $ lbsNE)

randomSaturation :: MonadRandom m => Hue -> Luminosity -> m Int
randomSaturation HueMonochrome _   = return 0

randomSaturation hue           lum = case lum of
  LumRandom -> getRandomR (0, 100)
  LumBright -> getRandomR (55, hi)
  LumDark   -> getRandomR (hi - 10, hi)
  LumLight  -> getRandomR (lo, 55)
  where
    (lo, hi) = saturationRange hue

minBrightness :: Hue -> Int -> Int
minBrightness hue saturationValue = round $ fromMaybe 0 result
  where
    lbs = lowerBounds $ getColorDefinition hue
    tup a  = zip (0:a) a
    inRange j (k, n) = j >= k && j <= n
    result :: Maybe Double
    result = do
      (s1, s2) <- find (inRange saturationValue) (tup $ fmap fst lbs)
      v1       <- lookup s1 lbs
      v2       <- lookup s2 lbs
      let m = fromIntegral (v2 - v1) / fromIntegral (s2 -s1)
          b = fromIntegral v1 - m * fromIntegral s1
      return $ m * fromIntegral saturationValue + b

-- | Pick a random brightness value given a 'Hue', 'Luminosity' and saturation.
randomBrightness :: MonadRandom m => Hue -> Luminosity -> Int -> m Int
randomBrightness hue lum saturationValue = getRandomR (bMin, bMax)
  where
    b            = minBrightness hue saturationValue
    (bMin, bMax) = case lum of
      LumBright -> (b, 100)
      LumDark   -> (b, b + 20)
      LumLight  -> ((b + 100) `div` 2, 100)
      LumRandom -> (0, 100)

-- | Generate a random opaque color.
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_red.svg>>
--
-- > randomColor HueRed LumBright
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_orange.svg>>
--
-- > randomColor HueOrange LumBright
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_yellow.svg>>
--
-- > randomColor HueYellow LumBright
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_green.svg>>
--
-- > randomColor HueGreen LumBright
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_blue.svg>>
--
-- > randomColor HueBlue LumBright
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_purple.svg>>
--
-- > randomColor HuePurple LumBright
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_pink.svg>>
--
-- > randomColor HuePink LumBright
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_monochrome.svg>>
--
-- > randomColor HueMonochrome LumRandom
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_light.svg>>
--
-- > randomColor HueRandom LumLight
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_dark.svg>>
--
-- > randomColor HueRandom LumDark
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_random.svg>>
--
-- > randomColor HueRandom LumRandom
-- /Better to use 'randomCIELab' for truly random colors/.
randomColor :: MonadRandom m => Hue -> Luminosity -> m Kolor
randomColor hue lum = do
  hueValue <- randomHue hue
  let hue'  = getHue hueValue
  satValue <- randomSaturation hue' lum
  briValue <- randomBrightness hue' lum satValue
  let (RGB r g b) = hsv (fromIntegral hueValue)
                        (fromIntegral satValue / 100)
                        (fromIntegral briValue / 100)
  return $ sRGB r g b

-- | Return a random harmony based on a seed color.
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

-- | Generate a random color palette. First choose a random color then choose a
--   random harmony and apply it.
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_palette.svg>>
--
-- > randomPalette
randomPalette :: MonadRandom m => Hue -> Luminosity ->m [Kolor]
randomPalette hue lum = randomColor hue lum >>= randomHarmony

-- | Generate a random color from CIELAB (a perceptually uniform color space)
--   with a White point of 'd65'. Probably the best choice if you want a totally
--   random color.
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_cielab.svg>>
--
-- > randomCIELab
randomCIELab :: MonadRandom m => m Kolor
randomCIELab = do
  l <- getRandomR (0, 100)
  a <- getRandomR (-100, 100)
  b <- getRandomR (-100, 100)
  return $ cieLAB d65 l a b

-- | Generate a random color palette using 'randomCIELab'. First choose a
--   random color then choose a random harmony and apply it.
--
-- <<diagrams/src_Data_Colour_Palette_RandomColor_labpalette.svg>>
--
-- > randomCIELabPalette
randomCIELabPalette :: MonadRandom m => m [Kolor]
randomCIELabPalette = randomCIELab >>= randomHarmony
