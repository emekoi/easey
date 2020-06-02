module Easey.Tween () where

import Data.Maybe (fromMaybe)
import Easey

data Easing
  = InSine
  | OutSine
  | InOutSine
  | InQuad
  | OutQuad
  | InOutQuad
  | InCubic
  | OutCubic
  | InOutCubic
  | InQuartic
  | OutQuartic
  | InOutQuartic
  | InQuintic
  | OutQuintic
  | InOutQuintic
  | InExpo
  | OutExpo
  | InOutExpo
  | InCirc
  | OutCirc
  | InOutCirc
  | InBack
  | OutBack
  | InOutBack
  | InElastic
  | OutElastic
  | InOutElastic
  | InBounce
  | OutBounce
  | InOutBounce

ease :: (Floating a, Ord a) => Easing -> (a -> a)
ease InSine = inSine
ease OutSine = outSine
ease InOutSine = inOutSine
ease InQuad = inQuad
ease OutQuad = outQuad
ease InOutQuad = inOutQuad
ease InCubic = inCubic
ease OutCubic = outCubic
ease InOutCubic = inOutCubic
ease InQuartic = inQuartic
ease OutQuartic = outQuartic
ease InOutQuartic = inOutQuartic
ease InQuintic = inQuintic
ease OutQuintic = outQuintic
ease InOutQuintic = inOutQuintic
ease InExpo = inExpo
ease OutExpo = outExpo
ease InOutExpo = inOutExpo
ease InCirc = inCirc
ease OutCirc = outCirc
ease InOutCirc = inOutCirc
ease InBack = inBack
ease OutBack = outBack
ease InOutBack = inOutBack
ease InElastic = inElastic
ease OutElastic = outElastic
ease InOutElastic = inOutElastic
ease InBounce = inBounce
ease OutBounce = outBounce
ease InOutBounce = inOutBounce
{-# INLINE ease #-}

data Tween a = Tween
  { _easing :: a -> a,
    _progress :: !a,
    _rate :: !a,
    _diff :: !a,
    _start :: !a,
    _after :: Maybe (Tween a)
  }

instance Semigroup (Tween a) where
  t1 <> t2 = t1 {_after = Just t2}

instance Floating a => Monoid (Tween a) where
  mempty =
    Tween
      { _easing = id,
        _progress = 1.0,
        _start = 0,
        _rate = 0,
        _diff = 0,
        _after = Nothing
      }

tweenTo :: (Floating a, Ord a) => Easing -> a -> a -> a -> Tween a
tweenTo e t a a' =
  Tween
    { _easing = ease e,
      _progress = if t > 0 then 0 else 1,
      _rate = if t > 0 then 1 / t else 0,
      _diff = a' - a,
      _start = a,
      _after = Nothing
    }

updateTween :: (Floating a, Ord a) => Tween a -> a -> (Tween a, a)
updateTween t dt = (t', _start t + m * _diff t)
  where
    p' = if _progress t < 1 then _progress t + _rate t * dt else 1
    m = if p' >= 1 then 1 else _easing t p'
    t' =
      if p' >= 1
        then fromMaybe t {_progress = p', _rate = 0} (_after t)
        else t {_progress = p'}
