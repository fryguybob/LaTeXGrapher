module LaTeXGrapher.Data.Expression
    (
      Expr(..)
    , fromBinary
    , fromUnary
    ) where
    
import Data.List(intersperse)
import Data.Char(isLetter)

-- Abstract syntax tree for expressions
data Expr = ExprFunc String [Expr] | Const Double

fromBinary :: String -> Expr -> Expr -> Expr
fromBinary n a b = ExprFunc n [a,b]

fromUnary :: String -> Expr -> Expr
fromUnary n a = ExprFunc n [a]

instance Show Expr where
    show (ExprFunc n es) | isOperator n = showOp n es
                         | otherwise    = showFunc n es
    show (Const d)       = show d

showOp n (a:b:[]) = "(" ++ show a ++ " " ++ n ++ " " ++ show b ++ ")"
showOp n es       = "(" ++ n ++ ")(" ++ concat (intersperse "," $ map show es) ++ ")"

showFunc n [] = n
showFunc n es = concat $ n : "(" : (intersperse "," $ map show es) ++ [")"]


isOperator (a:_) = (not . isLetter) a
isOperator _     = False