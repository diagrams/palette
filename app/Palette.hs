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
main = mainWith diagram

diagram :: Int -> Diagram B
diagram seed = vsep 0.05 (dots <$> [hs, hp, cs, ds, es, ep])
  where
    hs = evalRand (replicateM 12 randomCIELab) (mkStdGen seed)
    hp = evalRand randomCIELabPalette (mkStdGen seed)
    cs = evalRand (replicateM 12 $ randomColor HueMonochrome LumRandom) (mkStdGen seed)
    ds = evalRand (replicateM 12 $ randomColor HueRed LumBright) (mkStdGen seed)
    es = evalRand (replicateM 12 $ randomColor HueRandom LumBright) (mkStdGen seed)
    ep = evalRand (randomPalette HueRandom LumBright) (mkStdGen seed)


dots :: [Kolor] -> Diagram B
dots cs = hsep 0.5 ((\c -> circle 1 # fc c) <$> cs)
           # lw none
           # frame 0.5
