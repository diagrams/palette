{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.RandomColor
import Data.Colour.Palette.Types
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant
import Control.Monad.Random

main :: IO ()
-- main = mainWith $ diagram HueRandom LumRandom
main = mainWith $ \seed -> dots $ evalRand (randomPalette HueRandom LumBright) (mkStdGen seed)

diagram :: Hue -> Luminosity -> Int -> Diagram B
diagram h l seed = dots cs
  where
    -- hs = evalRand (replicateM 10 randomCIELab) (mkStdGen seed)
    -- hp = evalRand randomCIELabPalette (mkStdGen seed)
    cs = evalRand (replicateM 10 $ randomColor h l) (mkStdGen seed)
    ds = evalRand (replicateM 10 $ randomColor h l) (mkStdGen $ seed + 1)
    es = evalRand (replicateM 10 $ randomColor h l) (mkStdGen $ seed + 2)
    -- ep = evalRand (randomPalette HueRandom LumBright) (mkStdGen seed)


dots :: [Kolor] -> Diagram B
dots cs = hsep 0.5 ((\c -> circle 1 # fc c) <$> cs)
           # lw none
           # frame 0.5
