module Easey
  ( inSine,
    outSine,
    inOutSine,
    inQuad,
    outQuad,
    inOutQuad,
    inCubic,
    outCubic,
    inOutCubic,
    inQuartic,
    outQuartic,
    inOutQuartic,
    inQuintic,
    outQuintic,
    inOutQuintic,
    inExpo,
    outExpo,
    inOutExpo,
    inCirc,
    outCirc,
    inOutCirc,
    inBack,
    outBack,
    inOutBack,
    inElastic,
    outElastic,
    inOutElastic,
    inBounce,
    outBounce,
    inOutBounce,
  )
where

import Control.Monad (ap, liftM2)
import Easey.Internals

-- Cheat Sheet: https://easings.net/en#easeInSine
inSine :: Floating a => a -> a
inSine = (1 -) . cos . (* (pi / 2))

-- Cheat Sheet: https://easings.net/en#easeOutSine
outSine :: Floating a => a -> a
outSine = toOut inSine

-- Cheat Sheet: https://easings.net/en#easeInOutSine
inOutSine :: (Floating a, Ord a) => a -> a
inOutSine = toInOut inSine

-- Cheat Sheet: https://easings.net/en#easeInQuad
inQuad :: Floating a => a -> a
inQuad = inPoly 2

-- Cheat Sheet: https://easings.net/en#easeOutQuad
outQuad :: Floating a => a -> a
outQuad = outPoly 2

-- Cheat Sheet: https://easings.net/en#easeInOutQuad
inOutQuad :: (Floating a, Ord a) => a -> a
inOutQuad = inOutPoly 2

-- Cheat Sheet: https://easings.net/en#easeInCubic
inCubic :: Floating a => a -> a
inCubic = inPoly 3

-- Cheat Sheet: https://easings.net/en#easeOutCubic
outCubic :: Floating a => a -> a
outCubic = outPoly 3

-- Cheat Sheet: https://easings.net/en#easeInOutCubic
inOutCubic :: (Floating a, Ord a) => a -> a
inOutCubic = inOutPoly 3

-- Cheat Sheet: https://easings.net/en#easeInQuartic
inQuartic :: Floating a => a -> a
inQuartic = inPoly 4

-- Cheat Sheet: https://easings.net/en#easeOutQuartic
outQuartic :: Floating a => a -> a
outQuartic = outPoly 4

-- Cheat Sheet: https://easings.net/en#easeInOutQuartic
inOutQuartic :: (Floating a, Ord a) => a -> a
inOutQuartic = inOutPoly 4

-- Cheat Sheet: https://easings.net/en#easeInQuintic
inQuintic :: Floating a => a -> a
inQuintic = inPoly 5

-- Cheat Sheet: https://easings.net/en#easeOutQuintic
outQuintic :: Floating a => a -> a
outQuintic = outPoly 5

-- Cheat Sheet: https://easings.net/en#easeInOutQuintic
inOutQuintic :: (Floating a, Ord a) => a -> a
inOutQuintic = inOutPoly 5

-- Cheat Sheet: https://easings.net/en#easeInExpo
inExpo :: (Floating a, Eq a) => a -> a
inExpo 0 = 0
inExpo t = (2 **) . (10 *) . subtract 1 $ t

-- Cheat Sheet: https://easings.net/en#easeOutExpo
outExpo :: (Floating a, Eq a) => a -> a
outExpo 1 = 1
outExpo t = toOut inExpo t

-- Cheat Sheet: https://easings.net/en#easeInOutExpo
inOutExpo :: (Floating a, Ord a) => a -> a
inOutExpo = toInOut inExpo

-- Cheat Sheet: https://easings.net/en#easeInCirc
inCirc :: Floating a => a -> a
inCirc = (1 -) . sqrt . (1 -) . (^^ 2)

-- Cheat Sheet: https://easings.net/en#easeOutCirc
outCirc :: Floating a => a -> a
outCirc = toOut inCirc

-- Cheat Sheet: https://easings.net/en#easeInOutCirc
inOutCirc :: (Floating a, Ord a) => a -> a
inOutCirc = toInOut inCirc

-- Cheat Sheet: https://easings.net/en#easeInBack
inBack :: Floating a => a -> a
inBack = liftM2 (*) (^^ 2) (subtract c1 . (c3 *))
  where
    c1 = 1.70158
    c3 = c1 + 1

-- Cheat Sheet: https://easings.net/en#easeOutBack
outBack :: Floating a => a -> a
outBack = toOut inBack

-- Cheat Sheet: https://easings.net/en#easeInOutBack
inOutBack :: (Floating a, Ord a) => a -> a
inOutBack = toInOut inBack

-- Cheat Sheet: https://easings.net/en#easeInElastic
inElastic :: (Eq a, Floating a) => a -> a
inElastic 0 = 0
inElastic 1 = 1
-- inElastic = negate . ap ((*) . (2 **) . (10 *) . subtract 1) (sin . (c4 *) . subtract 1.075)
inElastic t = - (2 ** (10 * (t - 1)) * sin ((t - 1.075) * c4))
  where
    c4 = (2 * pi) / 0.3

-- Cheat Sheet: https://easings.net/en#easeOutElastic
outElastic :: (Eq a, Floating a) => a -> a
outElastic = toOut inElastic

-- Cheat Sheet: https://easings.net/en#easeInOutElastic
inOutElastic :: (Floating a, Ord a) => a -> a
inOutElastic = toInOut inElastic

-- Cheat Sheet: https://easings.net/en#easeInBounce
inBounce :: (Floating a, Ord a) => a -> a
inBounce = toOut outBounce

-- Cheat Sheet: https://easings.net/en#easeOutBounce
outBounce :: (Floating a, Ord a) => a -> a
outBounce t
  | t < 1 / d1 = f 0 0
  | t < 2 / d1 = f 1.5 0.75
  | t < 2.5 / d1 = f 2.25 0.9375
  | otherwise = f 2.625 0.984375
  where
    n1 = 7.5625
    d1 = 2.75
    f a b = n1 * (t - (a / d1)) * t + b

-- Cheat Sheet: https://easings.net/en#easeInOutBounce
inOutBounce :: (Floating a, Ord a) => a -> a
inOutBounce = toInOut outBounce
