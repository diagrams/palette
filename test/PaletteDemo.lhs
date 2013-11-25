Color Schemes and the Palette package
=====================================
Choosing a set of colors that look good together can be quite a challenge. The task comes up in a variety of different contexts including: webiste design, print design, cartography, and as we will discuss here, making diagrams. The problem comes down to a balancing act between two issues. First is that the choosen set of colors is asthetically pleasing and second is that there is enough contrast between them.

The easiest approach is to borrow a set of colors from someone who has already put in the time and effort to create it. The Palette package "Data.Colour.Palette.ColorSet" provides access to a few different predefined color sets including [the ones in d3](https://github.com/mbostock/d3/wiki/Ordinal-Scales) and the package "Data.Colour.Palette.BrewerSet" contains a large variety of color schemes created by Cynthia Brewer for use in map making see [COLORBREWER 2.0](http://colorbrewer2.org/).

Let's start out by builing some tools in the Diagrams Haskell EDSL.

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Data.Colour.SRGB (sRGB24read)
>
> import Data.Colour.Palette.Harmony
> import Data.Colour.Palette.ColorSet
> import Data.Colour.Palette.BrewerSet

Why not use the golden ration to give us some pleasing proportions?

> gr = 1.618

The function `bar` takes a list of colors which we define here as `[Colour Double]` and its type synonym `[Kolor]`. We set the length to the golden ration and the height to 1 and return a color bar diagram.

> bar cs = centerXY $ hcat [square gr # scaleX s # fc k # lw 0 | k <- cs]
>   where s = gr / (fromIntegral (length cs))

We can use `bar` to view the color sets in `Palette`. Let's make a few color bars. We will use functions provided in `Palette` to make the color lists and then use `bar` to make the diagrams. The function `d3Colors1` takes an `Int` between 0 and 9 and returns a color from the set.

> d3 = [d3Colors1 n | n <- [0..9]]
> ex1 = bar d3

`brewerSet` is the function we use to access schemes in the `Data.Colour.Palette.Brewerset` package. It takes two arguments the category of the set `ColorCat` and an integer representing how many colors in the set. The sets are divided up into 3 categories primarily for showing different types of data on a map: sequential, diverging and qualitative.

> ex2 = bar $ brewerSet GnBu 9    -- green/blue, sequential multihue
> ex3 = bar $ brewerSet PuOr 11   -- purple/orange, diverging
> ex4 = bar $ brewerSet Paired 11 -- qualitative

Some of the color sets provided occur with 2 or 4 brightness levels. `data Brightness = Darkest | Dark | Light | Lightest` with `Darkest == Dark` and similarly for `Light` when using a two set variant. The `grid` function is useful for visualizing these.

'grid' takes a nested list of colors `[[Kolor]]` and returns grid of vertically stacked bars.

> grid cs = centerXY $ vcat [bar c # scaleY s | c <- cs]
>   where s = 1 / (fromIntegral (length cs))

> d3Pairs = [[d3Colors2  Dark  n | n <- [0..9]], [d3Colors2 Light n | n <- [0..9]]]
> ex5 = grid d3Pairs

> d3Quads = [[d3Colors4 b n | n <- [0..9]] | b <- [Darkest, Dark, Light, Lightest]]
> ex6 = grid d3Quads

The are over 300 colors that W3C recommends that every browser support. These are usually list in alphabetical order, which needless to say does not separate similar colors will. `Palette` provides the function `webColors` which returns the $i$th color in a list which has first been sorted by hue and then travesed by skiping every $61$ elements. This cycles through a good amount of colors before repeating similar hues. The variant `infiniteWebColors` cycles the list. When using these colors a good practice is to pick some random starting point and increment the color number by 1 every time a new color is required.

> web = [[webColors (19 * j + i) | i <- [0..8]] | j <- [0..8]]
> ex7 = grid web

> web2 = [[webColors (19 * j + i) | i <- [0..19]] | j <- [0..14]]
> ex8 = grid web2

If none of the above color schemes suit your purposes (which seems pretty unlikely) or if you just want to create your own - use the functions in `Data.Colour.Palette.Harmony`. The functions provide a progammatic interface to tools similar to [Adobe Kuler](https://kuler.adobe.com/create/color-wheel/) and [Color Scheme Designer](http://colorschemedesigner.com/).

Insert description of color harmony ryb, etc ...

The RYB color wheel and the color schemes we will design below are easy to view using the `wheel` function. `wheel` takes a list of colors and makes a color wheel out of them by placing the first color in the list at the top of the wheel.

> wheel cs = wheel' # rotateBy r
>   where
>     wheel' = mconcat $ zipWith fc cs (iterateN n (rotateBy a) w)
>     n = length cs
>     a = 1 / (fromIntegral n) :: Turn
>     w = wedge 1 (0 :: Turn) a # lw 0
>     r = 1/4 - 1/(2*(fromIntegral n))

Here is the RYB color wheel. Notice that colors we percieve as opposites, e.g. red and green are 180 degrees apart on the wheel.

> ryb = [rybColor n | n <- [0..23]]
> ex9 = wheel ryb

Let's pick a base color to demonstrate how to use the `Harmony` functiions. How about a nice mustardy yellow "#FFC200".

> base = sRGB24read "#FFC200"

> ex10 = wheel $ monochrome base
> ex11 = wheel $ complement base
> ex12 = wheel $ bwg base
> ex13 = pie $ triad base
> ex14 = pie $ tetrad base
> ex15 = pie $ analogic base
> ex16 = pie $ accentAnalogic base

> pie [] = circle 1 # fc black
> pie (c:cs) = ring <> center
>  where
>    center = circle 0.5 # fc c # lw 0
>    ring = mconcat $ zipWith fc cs (iterateN n (rotateBy a) w)
>    n = length cs
>    a = 1 / (fromIntegral n) :: Turn
>    w = annularWedge 0.5 1 (0 :: Turn) a # lw 0

> sx = strutX 0.5
> sy = strutY 0.5

> main = defaultMain $ ( ex1 === sy
>                  === ( ex2 ||| sx ||| ex3 ||| sx ||| ex4) === sy
>                  === ( ex5 ||| sx ||| ex6) === sy
>                  === ( ex7 ||| sx ||| ex8) === sy
>                  === ( ex9 ) === sy
>                  === ( ex10 ||| sx ||| ex11 ||| sx ||| ex12) === sy
>                  === ( ex13 ||| sx ||| ex14) === sy
>                  === ( ex15 ||| sx ||| ex16)
>                      ) # pad 1.1