-----------------------------------------------------------------------------
-- |
-- Module      :  Palette.Brewer
-- Copyright   :  (c) 2013 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Sets of between 3 and 12 colors.
-- This product includes color specifications and designs developed by
-- Cynthia Brewer (http://colorbrewer.org/).
--
-----------------------------------------------------------------------------

module Data.Colour.Palette.BrewerSet
      ( -- * Color schemes from Cynthia Brewer.
          brewerSet
        , Kolor
        , ColorCat(..)
      ) where

import qualified Data.Map.Lazy as M
import           Data.Colour
import           Data.Colour.SRGB         (sRGB24read)

type Kolor = Colour Double

-- | Categories of color sets. Each category has several lists of colors.
--   Each one containing the number of colors in the range specfied.
data ColorCat = YlGn      -- ^ 3 - 9,  sequential multihue
              | YlGnBu    -- ^ 3 - 9,  sequential multihue
              | GnBu      -- ^ 3 - 9,  sequential multihue
              | BuGn      -- ^ 3 - 9,  sequential multihue
              | PuBuGn    -- ^ 3 - 9,  sequential multihue
              | PuBu      -- ^ 3 - 9,  sequential multihue
              | BuPu      -- ^ 3 - 9,  sequential multihue
              | RdPu      -- ^ 3 - 9,  sequential multihue
              | PuRd      -- ^ 3 - 9,  sequential multihue
              | OrRd      -- ^ 3 - 9,  sequential multihue
              | YlOrRd    -- ^ 3 - 9,  sequential multihue
              | YlOrBr    -- ^ 3 - 9,  sequential multihue
              | Purples   -- ^ 3 - 9,  sequential single hue
              | Blues     -- ^ 3 - 9,  sequential single hue
              | Greens    -- ^ 3 - 9,  sequential single hue
              | Oranges   -- ^ 3 - 9,  sequential single hue
              | Reds      -- ^ 3 - 9,  sequential single hue
              | Greys     -- ^ 3 - 9,  sequential single hue
              | PuOr      -- ^ 3 - 11, diverging
              | BrBG      -- ^ 3 - 11, diverging
              | PRGn      -- ^ 3 - 11, diverging
              | PiYG      -- ^ 3 - 11, diverging
              | RdBu      -- ^ 3 - 11, diverging
              | RdGy      -- ^ 3 - 11, diverging
              | RdYlBu    -- ^ 3 - 11, diverging
              | Spectral  -- ^ 3 - 11, diverging
              | RdYlGn    -- ^ 3 - 11, diverging
              | Accent    -- ^ 3 - 8,  qualitative
              | Dark2     -- ^ 3 - 8,  qualitative
              | Paired    -- ^ 3 - 12, qualitative
              | Pastel1   -- ^ 3 - 9,  qualitative
              | Pastel2   -- ^ 3 - 8,  qualitative
              | Set1      -- ^ 3 - 9,  qualitative
              | Set2      -- ^ 3 - 8,  qualitative
              | Set3      -- ^ 3 - 12, qualitative
  deriving (Eq, Ord)

data ColorSet = ColorSet ColorCat Int deriving (Eq, Ord)

-- | Obtain a list of colors for the color scheme designated by category and
--   number `n` of colors in the theme. If the category and/or number does not
--   exist then returns a list `black` repeated `n` times.
brewerSet :: ColorCat -> Int -> [Kolor]
brewerSet cat n = maybe (replicate n black) (map sRGB24read) b
  where
    b = M.lookup (ColorSet cat n) brewerColor

brewerColor :: M.Map ColorSet [String]
brewerColor = M.fromList brewer

-- Color set data -----------------------------------------------------------------------
-------------------------------------------------------------------------------

brewer :: [(ColorSet, [String])]
brewer = [
    (ColorSet Spectral 11, ["#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b","#ffffbf","#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"]),
    (ColorSet Spectral 10, ["#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"]),
    (ColorSet Spectral 3, ["#fc8d59","#ffffbf","#99d594"]),
    (ColorSet Spectral 5, ["#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"]),
    (ColorSet Spectral 4, ["#d7191c","#fdae61","#abdda4","#2b83ba"]),
    (ColorSet Spectral 7, ["#d53e4f","#fc8d59","#fee08b","#ffffbf","#e6f598","#99d594","#3288bd"]),
    (ColorSet Spectral 6, ["#d53e4f","#fc8d59","#fee08b","#e6f598","#99d594","#3288bd"]),
    (ColorSet Spectral 9, ["#d53e4f","#f46d43","#fdae61","#fee08b","#ffffbf","#e6f598","#abdda4","#66c2a5","#3288bd"]),
    (ColorSet Spectral 8, ["#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5","#3288bd"]),
    (ColorSet RdYlGn 11, ["#a50026","#d73027","#f46d43","#fdae61","#fee08b","#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850","#006837"]),
    (ColorSet RdYlGn 10, ["#a50026","#d73027","#f46d43","#fdae61","#fee08b","#d9ef8b","#a6d96a","#66bd63","#1a9850","#006837"]),
    (ColorSet RdYlGn 3, ["#fc8d59","#ffffbf","#91cf60"]),
    (ColorSet RdYlGn 5, ["#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641"]),
    (ColorSet RdYlGn 4, ["#d7191c","#fdae61","#a6d96a","#1a9641"]),
    (ColorSet RdYlGn 7, ["#d73027","#fc8d59","#fee08b","#ffffbf","#d9ef8b","#91cf60","#1a9850"]),
    (ColorSet RdYlGn 6, ["#d73027","#fc8d59","#fee08b","#d9ef8b","#91cf60","#1a9850"]),
    (ColorSet RdYlGn 9, ["#d73027","#f46d43","#fdae61","#fee08b","#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850"]),
    (ColorSet RdYlGn 8, ["#d73027","#f46d43","#fdae61","#fee08b","#d9ef8b","#a6d96a","#66bd63","#1a9850"]),
    (ColorSet Set2 3, ["#66c2a5","#fc8d62","#8da0cb"]),
    (ColorSet Set2 5, ["#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854"]),
    (ColorSet Set2 4, ["#66c2a5","#fc8d62","#8da0cb","#e78ac3"]),
    (ColorSet Set2 7, ["#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f","#e5c494"]),
    (ColorSet Set2 6, ["#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f"]),
    (ColorSet Set2 8, ["#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f","#e5c494","#b3b3b3"]),
    (ColorSet Accent 3, ["#7fc97f","#beaed4","#fdc086"]),
    (ColorSet Accent 5, ["#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0"]),
    (ColorSet Accent 4, ["#7fc97f","#beaed4","#fdc086","#ffff99"]),
    (ColorSet Accent 7, ["#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f","#bf5b17"]),
    (ColorSet Accent 6, ["#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f"]),
    (ColorSet Accent 8, ["#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f","#bf5b17","#666666"]),
    (ColorSet OrRd 3, ["#fee8c8","#fdbb84","#e34a33"]),
    (ColorSet OrRd 5, ["#fef0d9","#fdcc8a","#fc8d59","#e34a33","#b30000"]),
    (ColorSet OrRd 4, ["#fef0d9","#fdcc8a","#fc8d59","#d7301f"]),
    (ColorSet OrRd 7, ["#fef0d9","#fdd49e","#fdbb84","#fc8d59","#ef6548","#d7301f","#990000"]),
    (ColorSet OrRd 6, ["#fef0d9","#fdd49e","#fdbb84","#fc8d59","#e34a33","#b30000"]),
    (ColorSet OrRd 9, ["#fff7ec","#fee8c8","#fdd49e","#fdbb84","#fc8d59","#ef6548","#d7301f","#b30000","#7f0000"]),
    (ColorSet OrRd 8, ["#fff7ec","#fee8c8","#fdd49e","#fdbb84","#fc8d59","#ef6548","#d7301f","#990000"]),
    (ColorSet Set1 3, ["#e41a1c","#377eb8","#4daf4a"]),
    (ColorSet Set1 5, ["#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00"]),
    (ColorSet Set1 4, ["#e41a1c","#377eb8","#4daf4a","#984ea3"]),
    (ColorSet Set1 7, ["#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628"]),
    (ColorSet Set1 6, ["#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33"]),
    (ColorSet Set1 9, ["#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf","#999999"]),
    (ColorSet Set1 8, ["#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf"]),
    (ColorSet PuBu 3, ["#ece7f2","#a6bddb","#2b8cbe"]),
    (ColorSet PuBu 5, ["#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d"]),
    (ColorSet PuBu 4, ["#f1eef6","#bdc9e1","#74a9cf","#0570b0"]),
    (ColorSet PuBu 7, ["#f1eef6","#d0d1e6","#a6bddb","#74a9cf","#3690c0","#0570b0","#034e7b"]),
    (ColorSet PuBu 6, ["#f1eef6","#d0d1e6","#a6bddb","#74a9cf","#2b8cbe","#045a8d"]),
    (ColorSet PuBu 9, ["#fff7fb","#ece7f2","#d0d1e6","#a6bddb","#74a9cf","#3690c0","#0570b0","#045a8d","#023858"]),
    (ColorSet PuBu 8, ["#fff7fb","#ece7f2","#d0d1e6","#a6bddb","#74a9cf","#3690c0","#0570b0","#034e7b"]),
    (ColorSet Set3 11, ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5"]),
    (ColorSet Set3 10, ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd"]),
    (ColorSet Set3 12, ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5","#ffed6f"]),
    (ColorSet Set3 3, ["#8dd3c7","#ffffb3","#bebada"]),
    (ColorSet Set3 5, ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3"]),
    (ColorSet Set3 4, ["#8dd3c7","#ffffb3","#bebada","#fb8072"]),
    (ColorSet Set3 7, ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69"]),
    (ColorSet Set3 6, ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462"]),
    (ColorSet Set3 9, ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9"]),
    (ColorSet Set3 8, ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5"]),
    (ColorSet BuPu 3, ["#e0ecf4","#9ebcda","#8856a7"]),
    (ColorSet BuPu 5, ["#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"]),
    (ColorSet BuPu 4, ["#edf8fb","#b3cde3","#8c96c6","#88419d"]),
    (ColorSet BuPu 7, ["#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8c6bb1","#88419d","#6e016b"]),
    (ColorSet BuPu 6, ["#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8856a7","#810f7c"]),
    (ColorSet BuPu 9, ["#f7fcfd","#e0ecf4","#bfd3e6","#9ebcda","#8c96c6","#8c6bb1","#88419d","#810f7c","#4d004b"]),
    (ColorSet BuPu 8, ["#f7fcfd","#e0ecf4","#bfd3e6","#9ebcda","#8c96c6","#8c6bb1","#88419d","#6e016b"]),
    (ColorSet Dark2 3, ["#1b9e77","#d95f02","#7570b3"]),
    (ColorSet Dark2 5, ["#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e"]),
    (ColorSet Dark2 4, ["#1b9e77","#d95f02","#7570b3","#e7298a"]),
    (ColorSet Dark2 7, ["#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"]),
    (ColorSet Dark2 6, ["#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02"]),
    (ColorSet Dark2 8, ["#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d","#666666"]),
    (ColorSet RdBu 11, ["#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac","#053061"]),
    (ColorSet RdBu 10, ["#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#d1e5f0","#92c5de","#4393c3","#2166ac","#053061"]),
    (ColorSet RdBu 3, ["#ef8a62","#f7f7f7","#67a9cf"]),
    (ColorSet RdBu 5, ["#ca0020","#f4a582","#f7f7f7","#92c5de","#0571b0"]),
    (ColorSet RdBu 4, ["#ca0020","#f4a582","#92c5de","#0571b0"]),
    (ColorSet RdBu 7, ["#b2182b","#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf","#2166ac"]),
    (ColorSet RdBu 6, ["#b2182b","#ef8a62","#fddbc7","#d1e5f0","#67a9cf","#2166ac"]),
    (ColorSet RdBu 9, ["#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac"]),
    (ColorSet RdBu 8, ["#b2182b","#d6604d","#f4a582","#fddbc7","#d1e5f0","#92c5de","#4393c3","#2166ac"]),
    (ColorSet Oranges 3, ["#fee6ce","#fdae6b","#e6550d"]),
    (ColorSet Oranges 5, ["#feedde","#fdbe85","#fd8d3c","#e6550d","#a63603"]),
    (ColorSet Oranges 4, ["#feedde","#fdbe85","#fd8d3c","#d94701"]),
    (ColorSet Oranges 7, ["#feedde","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#8c2d04"]),
    (ColorSet Oranges 6, ["#feedde","#fdd0a2","#fdae6b","#fd8d3c","#e6550d","#a63603"]),
    (ColorSet Oranges 9, ["#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"]),
    (ColorSet Oranges 8, ["#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#8c2d04"]),
    (ColorSet BuGn 3, ["#e5f5f9","#99d8c9","#2ca25f"]),
    (ColorSet BuGn 5, ["#edf8fb","#b2e2e2","#66c2a4","#2ca25f","#006d2c"]),
    (ColorSet BuGn 4, ["#edf8fb","#b2e2e2","#66c2a4","#238b45"]),
    (ColorSet BuGn 7, ["#edf8fb","#ccece6","#99d8c9","#66c2a4","#41ae76","#238b45","#005824"]),
    (ColorSet BuGn 6, ["#edf8fb","#ccece6","#99d8c9","#66c2a4","#2ca25f","#006d2c"]),
    (ColorSet BuGn 9, ["#f7fcfd","#e5f5f9","#ccece6","#99d8c9","#66c2a4","#41ae76","#238b45","#006d2c","#00441b"]),
    (ColorSet BuGn 8, ["#f7fcfd","#e5f5f9","#ccece6","#99d8c9","#66c2a4","#41ae76","#238b45","#005824"]),
    (ColorSet PiYG 11, ["#8e0152","#c51b7d","#de77ae","#f1b6da","#fde0ef","#f7f7f7","#e6f5d0","#b8e186","#7fbc41","#4d9221","#276419"]),
    (ColorSet PiYG 10, ["#8e0152","#c51b7d","#de77ae","#f1b6da","#fde0ef","#e6f5d0","#b8e186","#7fbc41","#4d9221","#276419"]),
    (ColorSet PiYG 3, ["#e9a3c9","#f7f7f7","#a1d76a"]),
    (ColorSet PiYG 5, ["#d01c8b","#f1b6da","#f7f7f7","#b8e186","#4dac26"]),
    (ColorSet PiYG 4, ["#d01c8b","#f1b6da","#b8e186","#4dac26"]),
    (ColorSet PiYG 7, ["#c51b7d","#e9a3c9","#fde0ef","#f7f7f7","#e6f5d0","#a1d76a","#4d9221"]),
    (ColorSet PiYG 6, ["#c51b7d","#e9a3c9","#fde0ef","#e6f5d0","#a1d76a","#4d9221"]),
    (ColorSet PiYG 9, ["#c51b7d","#de77ae","#f1b6da","#fde0ef","#f7f7f7","#e6f5d0","#b8e186","#7fbc41","#4d9221"]),
    (ColorSet PiYG 8, ["#c51b7d","#de77ae","#f1b6da","#fde0ef","#e6f5d0","#b8e186","#7fbc41","#4d9221"]),
    (ColorSet YlOrBr 3, ["#fff7bc","#fec44f","#d95f0e"]),
    (ColorSet YlOrBr 5, ["#ffffd4","#fed98e","#fe9929","#d95f0e","#993404"]),
    (ColorSet YlOrBr 4, ["#ffffd4","#fed98e","#fe9929","#cc4c02"]),
    (ColorSet YlOrBr 7, ["#ffffd4","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"]),
    (ColorSet YlOrBr 6, ["#ffffd4","#fee391","#fec44f","#fe9929","#d95f0e","#993404"]),
    (ColorSet YlOrBr 9, ["#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#993404","#662506"]),
    (ColorSet YlOrBr 8, ["#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"]),
    (ColorSet YlGn 3, ["#f7fcb9","#addd8e","#31a354"]),
    (ColorSet YlGn 5, ["#ffffcc","#c2e699","#78c679","#31a354","#006837"]),
    (ColorSet YlGn 4, ["#ffffcc","#c2e699","#78c679","#238443"]),
    (ColorSet YlGn 7, ["#ffffcc","#d9f0a3","#addd8e","#78c679","#41ab5d","#238443","#005a32"]),
    (ColorSet YlGn 6, ["#ffffcc","#d9f0a3","#addd8e","#78c679","#31a354","#006837"]),
    (ColorSet YlGn 9, ["#ffffe5","#f7fcb9","#d9f0a3","#addd8e","#78c679","#41ab5d","#238443","#006837","#004529"]),
    (ColorSet YlGn 8, ["#ffffe5","#f7fcb9","#d9f0a3","#addd8e","#78c679","#41ab5d","#238443","#005a32"]),
    (ColorSet Pastel2 3, ["#b3e2cd","#fdcdac","#cbd5e8"]),
    (ColorSet Pastel2 5, ["#b3e2cd","#fdcdac","#cbd5e8","#f4cae4","#e6f5c9"]),
    (ColorSet Pastel2 4, ["#b3e2cd","#fdcdac","#cbd5e8","#f4cae4"]),
    (ColorSet Pastel2 7, ["#b3e2cd","#fdcdac","#cbd5e8","#f4cae4","#e6f5c9","#fff2ae","#f1e2cc"]),
    (ColorSet Pastel2 6, ["#b3e2cd","#fdcdac","#cbd5e8","#f4cae4","#e6f5c9","#fff2ae"]),
    (ColorSet Pastel2 8, ["#b3e2cd","#fdcdac","#cbd5e8","#f4cae4","#e6f5c9","#fff2ae","#f1e2cc","#cccccc"]),
    (ColorSet RdPu 3, ["#fde0dd","#fa9fb5","#c51b8a"]),
    (ColorSet RdPu 5, ["#feebe2","#fbb4b9","#f768a1","#c51b8a","#7a0177"]),
    (ColorSet RdPu 4, ["#feebe2","#fbb4b9","#f768a1","#ae017e"]),
    (ColorSet RdPu 7, ["#feebe2","#fcc5c0","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177"]),
    (ColorSet RdPu 6, ["#feebe2","#fcc5c0","#fa9fb5","#f768a1","#c51b8a","#7a0177"]),
    (ColorSet RdPu 9, ["#fff7f3","#fde0dd","#fcc5c0","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177","#49006a"]),
    (ColorSet RdPu 8, ["#fff7f3","#fde0dd","#fcc5c0","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177"]),
    (ColorSet Greens 3, ["#e5f5e0","#a1d99b","#31a354"]),
    (ColorSet Greens 5, ["#edf8e9","#bae4b3","#74c476","#31a354","#006d2c"]),
    (ColorSet Greens 4, ["#edf8e9","#bae4b3","#74c476","#238b45"]),
    (ColorSet Greens 7, ["#edf8e9","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45","#005a32"]),
    (ColorSet Greens 6, ["#edf8e9","#c7e9c0","#a1d99b","#74c476","#31a354","#006d2c"]),
    (ColorSet Greens 9, ["#f7fcf5","#e5f5e0","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b"]),
    (ColorSet Greens 8, ["#f7fcf5","#e5f5e0","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45","#005a32"]),
    (ColorSet PRGn 11, ["#40004b","#762a83","#9970ab","#c2a5cf","#e7d4e8","#f7f7f7","#d9f0d3","#a6dba0","#5aae61","#1b7837","#00441b"]),
    (ColorSet PRGn 10, ["#40004b","#762a83","#9970ab","#c2a5cf","#e7d4e8","#d9f0d3","#a6dba0","#5aae61","#1b7837","#00441b"]),
    (ColorSet PRGn 3, ["#af8dc3","#f7f7f7","#7fbf7b"]),
    (ColorSet PRGn 5, ["#7b3294","#c2a5cf","#f7f7f7","#a6dba0","#008837"]),
    (ColorSet PRGn 4, ["#7b3294","#c2a5cf","#a6dba0","#008837"]),
    (ColorSet PRGn 7, ["#762a83","#af8dc3","#e7d4e8","#f7f7f7","#d9f0d3","#7fbf7b","#1b7837"]),
    (ColorSet PRGn 6, ["#762a83","#af8dc3","#e7d4e8","#d9f0d3","#7fbf7b","#1b7837"]),
    (ColorSet PRGn 9, ["#762a83","#9970ab","#c2a5cf","#e7d4e8","#f7f7f7","#d9f0d3","#a6dba0","#5aae61","#1b7837"]),
    (ColorSet PRGn 8, ["#762a83","#9970ab","#c2a5cf","#e7d4e8","#d9f0d3","#a6dba0","#5aae61","#1b7837"]),
    (ColorSet YlGnBu 3, ["#edf8b1","#7fcdbb","#2c7fb8"]),
    (ColorSet YlGnBu 5, ["#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"]),
    (ColorSet YlGnBu 4, ["#ffffcc","#a1dab4","#41b6c4","#225ea8"]),
    (ColorSet YlGnBu 7, ["#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"]),
    (ColorSet YlGnBu 6, ["#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494"]),
    (ColorSet YlGnBu 9, ["#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58"]),
    (ColorSet YlGnBu 8, ["#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"]),
    (ColorSet RdYlBu 11, ["#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"]),
    (ColorSet RdYlBu 10, ["#a50026","#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"]),
    (ColorSet RdYlBu 3, ["#fc8d59","#ffffbf","#91bfdb"]),
    (ColorSet RdYlBu 5, ["#d7191c","#fdae61","#ffffbf","#abd9e9","#2c7bb6"]),
    (ColorSet RdYlBu 4, ["#d7191c","#fdae61","#abd9e9","#2c7bb6"]),
    (ColorSet RdYlBu 7, ["#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4"]),
    (ColorSet RdYlBu 6, ["#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb","#4575b4"]),
    (ColorSet RdYlBu 9, ["#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4"]),
    (ColorSet RdYlBu 8, ["#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"]),
    (ColorSet Paired 11, ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99"]),
    (ColorSet Paired 10, ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a"]),
    (ColorSet Paired 12, ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928"]),
    (ColorSet Paired 3, ["#a6cee3","#1f78b4","#b2df8a"]),
    (ColorSet Paired 5, ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99"]),
    (ColorSet Paired 4, ["#a6cee3","#1f78b4","#b2df8a","#33a02c"]),
    (ColorSet Paired 7, ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f"]),
    (ColorSet Paired 6, ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c"]),
    (ColorSet Paired 9, ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6"]),
    (ColorSet Paired 8, ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00"]),
    (ColorSet BrBG 11, ["#543005","#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e","#003c30"]),
    (ColorSet BrBG 10, ["#543005","#8c510a","#bf812d","#dfc27d","#f6e8c3","#c7eae5","#80cdc1","#35978f","#01665e","#003c30"]),
    (ColorSet BrBG 3, ["#d8b365","#f5f5f5","#5ab4ac"]),
    (ColorSet BrBG 5, ["#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"]),
    (ColorSet BrBG 4, ["#a6611a","#dfc27d","#80cdc1","#018571"]),
    (ColorSet BrBG 7, ["#8c510a","#d8b365","#f6e8c3","#f5f5f5","#c7eae5","#5ab4ac","#01665e"]),
    (ColorSet BrBG 6, ["#8c510a","#d8b365","#f6e8c3","#c7eae5","#5ab4ac","#01665e"]),
    (ColorSet BrBG 9, ["#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"]),
    (ColorSet BrBG 8, ["#8c510a","#bf812d","#dfc27d","#f6e8c3","#c7eae5","#80cdc1","#35978f","#01665e"]),
    (ColorSet Purples 3, ["#efedf5","#bcbddc","#756bb1"]),
    (ColorSet Purples 5, ["#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"]),
    (ColorSet Purples 4, ["#f2f0f7","#cbc9e2","#9e9ac8","#6a51a3"]),
    (ColorSet Purples 7, ["#f2f0f7","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#4a1486"]),
    (ColorSet Purples 6, ["#f2f0f7","#dadaeb","#bcbddc","#9e9ac8","#756bb1","#54278f"]),
    (ColorSet Purples 9, ["#fcfbfd","#efedf5","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"]),
    (ColorSet Purples 8, ["#fcfbfd","#efedf5","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#4a1486"]),
    (ColorSet Reds 3, ["#fee0d2","#fc9272","#de2d26"]),
    (ColorSet Reds 5, ["#fee5d9","#fcae91","#fb6a4a","#de2d26","#a50f15"]),
    (ColorSet Reds 4, ["#fee5d9","#fcae91","#fb6a4a","#cb181d"]),
    (ColorSet Reds 7, ["#fee5d9","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d"]),
    (ColorSet Reds 6, ["#fee5d9","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15"]),
    (ColorSet Reds 9, ["#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"]),
    (ColorSet Reds 8, ["#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d"]),
    (ColorSet Pastel1 3, ["#fbb4ae","#b3cde3","#ccebc5"]),
    (ColorSet Pastel1 5, ["#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6"]),
    (ColorSet Pastel1 4, ["#fbb4ae","#b3cde3","#ccebc5","#decbe4"]),
    (ColorSet Pastel1 7, ["#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd"]),
    (ColorSet Pastel1 6, ["#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc"]),
    (ColorSet Pastel1 9, ["#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd","#fddaec","#f2f2f2"]),
    (ColorSet Pastel1 8, ["#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd","#fddaec"]),
    (ColorSet GnBu 3, ["#e0f3db","#a8ddb5","#43a2ca"]),
    (ColorSet GnBu 5, ["#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac"]),
    (ColorSet GnBu 4, ["#f0f9e8","#bae4bc","#7bccc4","#2b8cbe"]),
    (ColorSet GnBu 7, ["#f0f9e8","#ccebc5","#a8ddb5","#7bccc4","#4eb3d3","#2b8cbe","#08589e"]),
    (ColorSet GnBu 6, ["#f0f9e8","#ccebc5","#a8ddb5","#7bccc4","#43a2ca","#0868ac"]),
    (ColorSet GnBu 9, ["#f7fcf0","#e0f3db","#ccebc5","#a8ddb5","#7bccc4","#4eb3d3","#2b8cbe","#0868ac","#084081"]),
    (ColorSet GnBu 8, ["#f7fcf0","#e0f3db","#ccebc5","#a8ddb5","#7bccc4","#4eb3d3","#2b8cbe","#08589e"]),
    (ColorSet Greys 3, ["#f0f0f0","#bdbdbd","#636363"]),
    (ColorSet Greys 5, ["#f7f7f7","#cccccc","#969696","#636363","#252525"]),
    (ColorSet Greys 4, ["#f7f7f7","#cccccc","#969696","#525252"]),
    (ColorSet Greys 7, ["#f7f7f7","#d9d9d9","#bdbdbd","#969696","#737373","#525252","#252525"]),
    (ColorSet Greys 6, ["#f7f7f7","#d9d9d9","#bdbdbd","#969696","#636363","#252525"]),
    (ColorSet Greys 9, ["#ffffff","#f0f0f0","#d9d9d9","#bdbdbd","#969696","#737373","#525252","#252525","#000000"]),
    (ColorSet Greys 8, ["#ffffff","#f0f0f0","#d9d9d9","#bdbdbd","#969696","#737373","#525252","#252525"]),
    (ColorSet RdGy 11, ["#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#ffffff","#e0e0e0","#bababa","#878787","#4d4d4d","#1a1a1a"]),
    (ColorSet RdGy 10, ["#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#e0e0e0","#bababa","#878787","#4d4d4d","#1a1a1a"]),
    (ColorSet RdGy 3, ["#ef8a62","#ffffff","#999999"]),
    (ColorSet RdGy 5, ["#ca0020","#f4a582","#ffffff","#bababa","#404040"]),
    (ColorSet RdGy 4, ["#ca0020","#f4a582","#bababa","#404040"]),
    (ColorSet RdGy 7, ["#b2182b","#ef8a62","#fddbc7","#ffffff","#e0e0e0","#999999","#4d4d4d"]),
    (ColorSet RdGy 6, ["#b2182b","#ef8a62","#fddbc7","#e0e0e0","#999999","#4d4d4d"]),
    (ColorSet RdGy 9, ["#b2182b","#d6604d","#f4a582","#fddbc7","#ffffff","#e0e0e0","#bababa","#878787","#4d4d4d"]),
    (ColorSet RdGy 8, ["#b2182b","#d6604d","#f4a582","#fddbc7","#e0e0e0","#bababa","#878787","#4d4d4d"]),
    (ColorSet YlOrRd 3, ["#ffeda0","#feb24c","#f03b20"]),
    (ColorSet YlOrRd 5, ["#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"]),
    (ColorSet YlOrRd 4, ["#ffffb2","#fecc5c","#fd8d3c","#e31a1c"]),
    (ColorSet YlOrRd 7, ["#ffffb2","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"]),
    (ColorSet YlOrRd 6, ["#ffffb2","#fed976","#feb24c","#fd8d3c","#f03b20","#bd0026"]),
    (ColorSet YlOrRd 9, ["#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026"]),
    (ColorSet YlOrRd 8, ["#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"]),
    (ColorSet PuOr 11, ["#7f3b08","#b35806","#e08214","#fdb863","#fee0b6","#f7f7f7","#d8daeb","#b2abd2","#8073ac","#542788","#2d004b"]),
    (ColorSet PuOr 10, ["#7f3b08","#b35806","#e08214","#fdb863","#fee0b6","#d8daeb","#b2abd2","#8073ac","#542788","#2d004b"]),
    (ColorSet PuOr 3, ["#f1a340","#f7f7f7","#998ec3"]),
    (ColorSet PuOr 5, ["#e66101","#fdb863","#f7f7f7","#b2abd2","#5e3c99"]),
    (ColorSet PuOr 4, ["#e66101","#fdb863","#b2abd2","#5e3c99"]),
    (ColorSet PuOr 7, ["#b35806","#f1a340","#fee0b6","#f7f7f7","#d8daeb","#998ec3","#542788"]),
    (ColorSet PuOr 6, ["#b35806","#f1a340","#fee0b6","#d8daeb","#998ec3","#542788"]),
    (ColorSet PuOr 9, ["#b35806","#e08214","#fdb863","#fee0b6","#f7f7f7","#d8daeb","#b2abd2","#8073ac","#542788"]),
    (ColorSet PuOr 8, ["#b35806","#e08214","#fdb863","#fee0b6","#d8daeb","#b2abd2","#8073ac","#542788"]),
    (ColorSet PuRd 3, ["#e7e1ef","#c994c7","#dd1c77"]),
    (ColorSet PuRd 5, ["#f1eef6","#d7b5d8","#df65b0","#dd1c77","#980043"]),
    (ColorSet PuRd 4, ["#f1eef6","#d7b5d8","#df65b0","#ce1256"]),
    (ColorSet PuRd 7, ["#f1eef6","#d4b9da","#c994c7","#df65b0","#e7298a","#ce1256","#91003f"]),
    (ColorSet PuRd 6, ["#f1eef6","#d4b9da","#c994c7","#df65b0","#dd1c77","#980043"]),
    (ColorSet PuRd 9, ["#f7f4f9","#e7e1ef","#d4b9da","#c994c7","#df65b0","#e7298a","#ce1256","#980043","#67001f"]),
    (ColorSet PuRd 8, ["#f7f4f9","#e7e1ef","#d4b9da","#c994c7","#df65b0","#e7298a","#ce1256","#91003f"]),
    (ColorSet Blues 3, ["#deebf7","#9ecae1","#3182bd"]),
    (ColorSet Blues 5, ["#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c"]),
    (ColorSet Blues 4, ["#eff3ff","#bdd7e7","#6baed6","#2171b5"]),
    (ColorSet Blues 7, ["#eff3ff","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#084594"]),
    (ColorSet Blues 6, ["#eff3ff","#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c"]),
    (ColorSet Blues 9, ["#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"]),
    (ColorSet Blues 8, ["#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#084594"]),
    (ColorSet PuBuGn 3, ["#ece2f0","#a6bddb","#1c9099"]),
    (ColorSet PuBuGn 5, ["#f6eff7","#bdc9e1","#67a9cf","#1c9099","#016c59"]),
    (ColorSet PuBuGn 4, ["#f6eff7","#bdc9e1","#67a9cf","#02818a"]),
    (ColorSet PuBuGn 7, ["#f6eff7","#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016450"]),
    (ColorSet PuBuGn 6, ["#f6eff7","#d0d1e6","#a6bddb","#67a9cf","#1c9099","#016c59"]),
    (ColorSet PuBuGn 9, ["#fff7fb","#ece2f0","#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"]),
    (ColorSet PuBuGn 8, ["#fff7fb","#ece2f0","#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016450"])]