.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

========================================
Introducing the Palette Package, Part II
========================================

.. contents::

A standard computer representation for color is RGB (red, green, blue). In fact most displays work by juxataposing red, green and blue elements. I turns out we can get any color by mixing these three colors (this is a bit of an exageration, for one thing RGB is device dependent). For most people thinking about a color as a combination of red, green and blue is not very intuitive. The HSV (hue, saturation, value) and HSL (hue, saturation, lightness) are attempts to transform the RGB coordinates of a color that is both intuitive and inexpensive to compute.

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
> ex9_05 = wheel $ map (tint 0.3) ryb

yada yada yada ...

> ex9_1 = bar $ map (rotateColor 60) d3

Let's pick a base color to demonstrate how to use the `Harmony` functions. How about a nice mustardy yellow "#FFC200".

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
