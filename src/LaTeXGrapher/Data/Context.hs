module LaTeXGrapher.Data.Context
    (
      Context(..)
    , minMax
    , defaultContext
    , lookupF
    , evaluateExpr
    , evaluateEPoint
    , evaluatePlotFunction
    ) where

import Control.Applicative ((<$>),(<*>))

import Data.Maybe
import Data.List
import Data.Map(fromList,Map)

import LaTeXGrapher.Data.Function
import LaTeXGrapher.Data.Expression
import LaTeXGrapher.Data.Markup
import LaTeXGrapher.Math
import LaTeXGrapher.Math.Bool

data Context = Context
    {
      plotFunctions :: [Function]
    , functions     :: [Function]
    , markups       :: [Markup]
    }

instance Show Context where
  show c = "Context: " 
        ++ (show $ minMax c) ++ ", " 
        ++ (show $ map name $ plotFunctions c) ++ ", " 
        ++ (show $ markups c)

instance MinMaxable Context where
  minMax c = def $ mmUnion ps
     where vs = filter isView (markups c)
           as = map toE vs
           ps = map (maybe (0,0) id . evaluateEPoint c) (concat as)
           toE (View es) = es
           def Nothing  = minMax ((-10,-10) :: Point) `include` (10,10)
           def (Just a) = a

lookupF :: Context -> String -> Maybe ([Double] -> Double)
lookupF c n = buildFunction c . functionData <$> find (isNamed n) (functions c)

evaluatePlotFunction :: Context -> Function -> Maybe ([Double], Double -> Double)
evaluatePlotFunction c f = (,) <$> ss <*> f'
  where f' = (\f x -> f [x]) <$> c `lookupF` (name f)
        ss = mapM (evaluateExpr c) (special . functionData $ f)

isNamed :: String -> Function -> Bool
isNamed n f = n == (name f)

buildFunction :: Context -> FunctionData -> [Double] -> Double
buildFunction _ (DefinedFunction f)   = f
buildFunction c (FunctionData e as _) 
  = \vs -> let args = argList as vs in
           evaluateWithArguments c e args

argList ks vs = zip ks vs

evaluateWithArguments :: Context -> [FExpression] -> [(String,Double)] -> Double
evaluateWithArguments _ []     _ = 0/0
evaluateWithArguments c (e:es) as = h $ evaluateFExpression c e as
                where h Nothing   = evaluateWithArguments c es as
                      h (Just v)  = v

evaluateFExpression :: Context -> FExpression -> [(String,Double)] -> Maybe Double
evaluateFExpression c e m = h (c `applyArguments` m) e
        where h c' (FExpression f) = evaluateExpr c' f
              h c' (ConditionalFExpression f p) = do
               b <- evaluateExpr c' p
               if toBool b
                 then evaluateExpr c' f
                 else Nothing

applyArguments :: Context -> [(String,Double)] -> Context
applyArguments c as = c { functions = fs ++ (functions c) }
           where fs = map (uncurry defineFunction) cs
                 cs = map f as
                 f (x,d) = (x, const d)

emptyContext = Context
  {
    plotFunctions = []
  , functions     = []
  , markups       = []
  }

defaultContext = emptyContext
  {
    functions = defaultFunctions
  }

defaultFunctions = map (uncurry defineFunction)
      [ ("*", bin (*))
      , ("+", bin (+))
      , ("-", bin (-))
      , ("/", bin (/))
      , ("^", bin (**))
      , ("||", bin dor)
      , ("&", bin dand)
      , ("&&", bin dand)
      , ("==", bin deq)
      , ("!=", bin dneq)
      , ("<=", bin dleq)
      , (">=", bin dgeq)
      , ("<", bin dl)
      , (">", bin dg)
      , ("neg", unary negate)
      , ("!", unary dnot)
      , ("sqrt", unary sqrt)
      , ("sin", unary sin)
      , ("cos", unary cos)
      , ("tan", unary sin)
      , ("arctan", unary atan)
      , ("arcsin", unary asin)
      , ("arccos", unary acos)
      , ("abs", unary abs)
      , ("floor", unary dfloor)
      , ("ceiling", unary dceiling)
      , ("ln", unary log)
      , ("log", unary (logBase 10))
      , ("odd", unary dodd)
      , ("even", unary deven)
      , ("pi", const pi)
      , ("e", const (exp 1))
      , ("NaN", const (0/0))
      ]

-- It would be nice to use the type system for this rather than []'ing things.
unary :: (a -> b) -> [a] -> b
unary f = \as -> f (head as)

bin :: (a -> a -> b) -> [a] -> b
bin f = \as -> g (head as, head $ tail as)
   where g = uncurry f

evaluateExpr :: Context -> Expr -> Maybe Double
evaluateExpr _ (Const d) = Just d
evaluateExpr c (ExprFunc n as) = ($) <$> c `lookupF` n <*> (sequence . map (evaluateExpr c) $ as)

evaluateEPoint :: Context -> EPoint -> Maybe Point
evaluateEPoint c (a,b) = (,) <$> evaluateExpr c a <*> evaluateExpr c b