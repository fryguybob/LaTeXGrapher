import LaTeXGrapher.Math
import LaTeXGrapher.Data.Function
import LaTeXGrapher.Data.Context
import LaTeXGrapher.Data.Markup
import LaTeXGrapher.Parser
import LaTeXGrapher.Parser.Expression
import LaTeXGrapher.Parser.Markup

import Text.ParserCombinators.Parsec
import Test.QuickCheck
import System.IO

t1 = "2+1"
t2 = "2 + 1"
t3 = "2.0+1.0"
t4 = "2.0 + 1.0"

full = "% includes all the points.\nView (-10,-10),(10, 10)\n% The function.\ng(x) = x     | x < -3\n"
fullTest = parseGrapher full

func = "g(x)=x|x<-3\n"
funcTest = parseGrapher func

multi = "% includes all the points.\nView (-10,-10),(10, 10)\n% The function.\n\ng(x) = x     | x < -3\n       x^2   | x < 0\n       x     |\n             : x = 1,2"
multiTest = parseGrapher multi

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse expr "expression" s

replS :: String -> IO ()
replS s = case parseGrapher s of
           Left e  -> putStrLn (show e)
           Right c -> do putStrLn "Context loaded. Enter \":q\" to quit."
                         repl c

repl :: Context -> IO ()
repl c = do putStr "> "
            l <- getLine
            if l == ":q"
              then return ()
              else eval l >> repl c
    where eval s = case parseExpr s of
                        Left e ->  putStrLn (show e)
                        Right e -> putStrLn $ "= " ++ (show $ evaluateExpr c e)

onlyRight :: (b -> Bool) -> Either a b -> Bool
onlyRight = either (\_ -> False)

rightAndEquals :: Eq b => b -> Either a b -> Bool
rightAndEquals b = onlyRight ((==) b)

prop_eval_constant :: Double -> Property
prop_eval_constant a = property $ rightAndEquals a $ evaluate (show a)

prop_eval_bin :: (OpData,Double,Double) -> Property
prop_eval_bin d = property $ testBinaryOp d

testBinaryOp :: (OpData, Double, Double) -> Bool
testBinaryOp d   = rightAndEquals ((opFunc op) rx ry) (evaluate s)
  where s        = (show x) ++ (opName op) ++ (show y)
        (op,x,y) = d
        v        = (x,y)
        rx       = (read :: String -> Double) $ show x
        ry       = (read :: String -> Double) $ show y

data OpData = Op { opName :: String, opFunc :: (Double -> Double -> Double) }

instance Show OpData where
  show d = show $ opName d

instance Arbitrary OpData where
  arbitrary 
    = do op <- elements 
           [ ("+",(+))
           , ("-", (-))
           -- , ("^", pow)
           , ("*", (*))
           -- , ("/", (/))
           ]
         s1 <- elements [ " ", "" ]
         s2 <- elements [ " ", "" ]
         return $ Op (s1 ++ (fst op) ++ s2) (snd op)