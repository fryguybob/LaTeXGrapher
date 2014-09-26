{-# LANGUAGE TypeSynonymInstances #-}

module LaTeXGrapher.Math
    (
      MinMax
    , mmMin
    , mmMax
    , mmUnion
    , mmContains
    , mmAdd
    , mmSub
    , mmWidth
    , mmHeight
    , inOpenInterval
    , inClosedInterval
    , includePoints
    , MinMaxable(..)
    , Point
    , dfloor
    , dceiling
    ) where

import Data.VectorSpace

type Point = (Double,Double)

data MinMax = MinMax Point Point

mmMin :: MinMax -> Point
mmMin (MinMax a _) = a

mmMax :: MinMax -> Point
mmMax (MinMax _ b) = b

mmWidth :: MinMax -> Double
mmWidth (MinMax a b) = fst (b ^-^ a)

mmHeight :: MinMax -> Double
mmHeight (MinMax a b) = snd (b ^-^ a)

mmAdd :: MinMax -> Point -> MinMax
mmAdd (MinMax a b) v = MinMax (a ^+^ v) (b ^+^ v)

mmSub :: MinMax -> Point -> MinMax
mmSub (MinMax a b) v = MinMax (a ^-^ v) (b ^-^ v)

mmContains :: MinMax -> Point -> Bool
mmContains (MinMax (ax,ay) (bx,by)) (px,py) = inClosedInterval px ax bx && inClosedInterval py ay by

inOpenInterval :: Double -> Double -> Double -> Bool
inOpenInterval x l u = x > l && x < u

inClosedInterval :: Double -> Double -> Double -> Bool
inClosedInterval x l u = x >= l && x <= u

class MinMaxable a where
  minMax  :: a -> MinMax
  include :: a -> Point -> MinMax
  
  include a p = MinMax c d
    where mm = minMax a
          (ax,ay) = mmMin mm
          (bx,by) = mmMax mm
          (x,y) = p
          c = (min ax x, min ay y)
          d = (max bx x, max by y)

mmUnion :: [Point] -> Maybe MinMax
mmUnion []     = Nothing
mmUnion (a:as) = Just $ minMax a `includePoints` as

includePoints :: MinMax -> [Point] -> MinMax
includePoints mm []     = mm
includePoints mm (b:bs) = (mm `include` b) `includePoints` bs

instance MinMaxable MinMax where
  minMax = id

instance MinMaxable Point where
  minMax p = MinMax p p

instance Show MinMax where
  show (MinMax a b) = (show a) ++ "-" ++ (show b)

dfloor :: Double -> Double
dfloor = fromIntegral . floor

dceiling :: Double -> Double
dceiling = fromIntegral . ceiling
