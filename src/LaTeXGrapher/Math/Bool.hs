module LaTeXGrapher.Math.Bool
    (
      dor
    , dodd
    , deven
    , dand
    , dgeq
    , dleq
    , deq
    , dneq
    , dg
    , dl
    , dnot
    , fromBool
    , toBool
    , toEitherBool
    ) where

toBool :: (Num a) => a -> Bool
toBool  a    = not (a == zero)
  where zero = fromInteger 0

toEitherBool :: Either a Double -> Either a Bool
toEitherBool (Right v) = Right (toBool v)
toEitherBool (Left v)  = Left v

fromBool :: Bool -> Double
fromBool True  = 1.0
fromBool False = 0.0

dBoolOp :: (Bool -> Bool -> Bool) -> Double -> Double -> Double
dBoolOp f = \a b -> fromBool $ f (toBool a) (toBool b)

dBoolRel :: (Double -> Double -> Bool) -> Double -> Double -> Double
dBoolRel f = curry $ fromBool . (uncurry f)

-- Doublized bool math operations
dor = dBoolOp (||)
dand = dBoolOp (&&)
dgeq = dBoolRel (>=)
dleq = dBoolRel (<=)
deq = dBoolRel (==)
dneq = dBoolRel (/=)
dg = dBoolRel (>)
dl = dBoolRel (<)
dnot a = fromBool $ not $ toBool a

dodd :: Double -> Double
dodd d = fromBool $ odd $ floor d

deven :: Double -> Double
deven d = fromBool $ even $ floor d
