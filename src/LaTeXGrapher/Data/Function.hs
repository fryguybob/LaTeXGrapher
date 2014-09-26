module LaTeXGrapher.Data.Function
    (
      Function(..)
    , FunctionData(..)
    , defineFunction
    , FExpression(..)
    , EPoint
    ) where

import Data.List(intersperse)

import LaTeXGrapher.Data.Expression
import LaTeXGrapher.Data.Tabular

data Function = Function
    {
      name         :: String
    , functionData :: FunctionData
    }

data FunctionData = FunctionData
    {
      expression :: [FExpression]
    , arguments  :: [String]
    , special    :: [Expr]
    }
    | DefinedFunction 
    {
      function :: [Double] -> Double
    }

data FExpression = FExpression Expr | ConditionalFExpression Expr Expr

type EPoint = (Expr,Expr)


defineFunction :: String -> ([Double] -> Double) -> Function
defineFunction n f = Function n (DefinedFunction f)

instance Show Function where
  show (Function n (DefinedFunction _))    = "(" ++ n ++ ")"
  show (Function n (FunctionData es as _)) = showTabular (rows es)
    where d = (n ++ "(" ++ (concat (intersperse ", " as)) ++ ") = ")
          rows :: [FExpression] -> [[String]]
          rows (v:vs) = h : ts
            where h  = d : (showFExpression v)
                  ts = ["" : (showFExpression x) | x <- vs]
instance Show FExpression where
  show e = concat (showFExpression e)

showFExpression (FExpression e) = [show e]
showFExpression (ConditionalFExpression e c) = [show e, " | " ++ show c]
