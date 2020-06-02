module Easey.Internals where

toOut :: Floating a => (a -> a) -> (a -> a)
toOut f = (1 -) . f . (1 -)

toInOut :: (Floating a, Ord a) => (a -> a) -> a -> a
toInOut f t
  | t' < 1.0 = 0.5 * f t'
  | otherwise = (0.5 +) . (0.5 *) . (1 -) $ f (2 - t')
  where
    t' = 2 * t

inPoly :: (Floating a, Integral b) => b -> a -> a
inPoly n = (^^ n)

outPoly :: (Floating a, Integral b) => b -> a -> a
outPoly n = toOut $ inPoly n

inOutPoly :: (Floating a, Ord a, Integral b) => b -> a -> a
inOutPoly n = toInOut $ inPoly n
