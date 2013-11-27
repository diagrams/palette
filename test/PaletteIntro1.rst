.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

=======================================
Introducing the Palette Package, Part I
=======================================

.. contents::

Introduction
============
Choosing a set of colors that look good together can be quite a challenge. The task comes up in a variety of different contexts including: webiste design, print design, cartography, and as we will discuss here, making diagrams. The problem comes down to a balancing act between two issues. First is that the choosen set of colors is asthetically pleasing and second is that there is enough contrast between them.

The easiest approach is to borrow a set of colors from someone who has already put in the time and effort to create it. The Palette package "Data.Colour.Palette.ColorSet" provides access to a few different predefined color sets including [the ones in d3](https://github.com/mbostock/d3/wiki/Ordinal-Scales) and the package "Data.Colour.Palette.BrewerSet" contains a large variety of color schemes created by Cynthia Brewer for use in map making see [COLORBREWER 2.0](http://colorbrewer2.org/).

Let's start out by builing some tools in the Diagrams Haskell EDSL.

.. class:: lhs

::

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


.. class::lhs

::

> gr = (1 + sqrt 5) / 2

The function `bar` takes a list of colors which we define here as `[Colour Double]` and its type synonym `[Kolor]`. We set the length to the golden ration and the height to 1 and return a color bar diagram.

.. class:: lhs

::

> bar cs = centerXY $ hcat [square gr # scaleX s # fc k # lw 0 | k <- cs]
>   where s = gr / (fromIntegral (length cs))

We can use `bar` to view the color sets in `Palette`. Let's make a few color bars. We will use functions provided in `Palette` to make the color lists and then use `bar` to make the diagrams. The function `d3Colors1` takes an `Int` between 0 and 9 and returns a color from the set.

.. class:: dia-lhs

::

> d3 = [d3Colors1 n | n <- [0..9]]
> example = bar d3

`brewerSet` is the function we use to access schemes in the `Data.Colour.Palette.Brewerset` package. It takes two arguments the category of the set `ColorCat` and an integer representing how many colors in the set. The sets are divided up into 3 categories primarily for showing different types of data on a map: sequential, diverging and qualitative.

.. class:: dia-lhs

::

> gb = bar $ brewerSet GnBu 9    -- green/blue, sequential multihue
> po = bar $ brewerSet PuOr 11   -- purple/orange, diverging
> bs = bar $ brewerSet Paired 11 -- qualitative
> example = hcat' (with & sep .~ 0.5) [gb, po, bs]

Some of the color sets provided occur with 2 or 4 brightness levels. `data Brightness = Darkest | Dark | Light | Lightest` with `Darkest == Dark` and similarly for `Light` when using a two set variant. The `grid` function is useful for visualizing these.

'grid' takes a nested list of colors `[[Kolor]]` and returns grid of vertically stacked bars.

.. class:: dia-lhs

::

> grid cs = centerXY $ vcat [bar c # scaleY s | c <- cs]
>   where s = 1 / (fromIntegral (length cs))
>
> d3Pairs = [[d3Colors2  Dark  n | n <- [0..9]], [d3Colors2 Light n | n <- [0..9]]]
> g2 = grid d3Pairs
>
> d3Quads = [[d3Colors4 b n | n <- [0..9]] | b <- [Darkest, Dark, Light, Lightest]]
> g4 = grid d3Quads
> example = hcat' (with & sep .~ 0.5) [g2, g4]

The are over 300 colors that W3C recommends that every browser support. These are usually list in alphabetical order, which needless to say does not separate similar colors will. `Palette` provides the function `webColors` which returns the $i$th color in a list which has first been sorted by hue and then travesed by skiping every $61$ elements. This cycles through a good amount of colors before repeating similar hues. The variant `infiniteWebColors` cycles the list. When using these colors a good practice is to pick some random starting point and increment the color number by 1 every time a new color is required.

.. class:: dia-lhs

::

> web = [[webColors (19 * j + i) | i <- [0..8]] | j <- [0..8]]
> w1 = grid web
>
> web2 = [[webColors (19 * j + i) | i <- [0..19]] | j <- [0..14]]
> w2 = grid web2
> example = hcat' (with & sep .~ 0.5) [w1, w2]

If none of the above color schemes suit your purposes or if you just want to create your own - use the functions in `Data.Colour.Palette.Harmony`. The module provides some basic functions for adjusting colors plus a progammatic interface to tools like [Adobe Kuler](https://kuler.adobe.com/create/color-wheel/) and [Color Scheme Designer](http://colorschemedesigner.com/). We'll finish Part 1 of this post by examining some of the functions provided to tweak a color: `shade`, `tone` and `tint`. These three functions mix a given color with black, gray, and white repsectively. So if for example we wanted a darker version of the d3 scheme, we can apply a shade.
Or we can add some gray to the brewer set `GnBu` from above.

.. class:: dia-lhs

::

> s = bar $ map (shade 0.75) d3
> t = bar $ map (tone 0.65) (brewerSet GnBu 9)
> example = hcat' (with & sep .~ 0.5) [s, t]