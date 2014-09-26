module LaTeXGrapher.Parser
    (
      parseGrapher
    , parseFunction
    , grapherStatements
    , insertContext
    ) where

import Data.Map
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import LaTeXGrapher.Data.Function
import LaTeXGrapher.Data.Context
import LaTeXGrapher.Data.Markup
import LaTeXGrapher.Parser.Expression
import LaTeXGrapher.Parser.Markup
import LaTeXGrapher.Parser.Grammer

parseGrapher :: String -> Either ParseError Context
parseGrapher s = parse grapherGrammer "Grapher" (s ++ "\n")

grapherGrammer :: Parser Context
grapherGrammer = do whiteSpace
                    ss <- many grapherStatements
                    eof
                    return $ buildContext ss

grapherStatements :: Parser (Either Markup ParsedFunction)
grapherStatements = do c <- choice [liftM Left parseMarkup, liftM Right parseFunction]
                       whiteSpace
                       return c

data FuncDecl = FuncDecl { isFuncPlot :: Bool, funcName :: String, funcArgs :: [String] }
                deriving(Show)

data ParsedFunction = ParsedFunction Bool Function
                      deriving(Show)

parseFunction :: Parser ParsedFunction
parseFunction = do d  <- try functionDecl
                   l  <- functionLine
                   ls <- many indentedFunctionLine
                   whiteSpace
                   s  <- option [] specialEol
                   return $ ParsedFunction (isFuncPlot d) $ buildFunction d (l:ls) s
        where indentedFunctionLine = do many spaceOrTab 
                                        functionLine
              specialEol = do s <- specialParse
                              eol
                              return s

functionDecl :: Parser FuncDecl
functionDecl = do b  <- option True notPlot
                  n  <- identifier
                  as <- option [] $ parens argList
                  many spaceOrTab
                  symbol "="
                  return $ FuncDecl b n as

spaceOrTab = (char ' ') <|> (char '\t')

notPlot :: Parser Bool
notPlot = do char '>'
             many spaceOrTab
             return False

argList :: Parser [String]
argList = sepBy1 identifier (char ',')

functionLine :: Parser FExpression
functionLine = do e <- expr
                  c <- option Nothing conditional
                  return $ buildFExpression e c

conditional :: Parser (Maybe Expr)
conditional = do char '|'
                 many spaceOrTab
                 option Nothing (liftM Just exprEol)

eol = (char '\n')

specialParse :: Parser [Expr]
specialParse = do char ':'
                  many spaceOrTab
                  identifier
                  many spaceOrTab
                  char '='
                  many spaceOrTab
                  exprList

buildFExpression :: Expr -> Maybe Expr -> FExpression
buildFExpression e Nothing  = FExpression e
buildFExpression e (Just c) = ConditionalFExpression e c

buildContext :: [Either Markup ParsedFunction] -> Context
buildContext = foldl insertContext defaultContext

buildFunction :: FuncDecl -> [FExpression] -> [Expr] -> Function
buildFunction d es ss = Function (funcName d) $ FunctionData es (funcArgs d) ss

insertContext :: Context -> Either Markup ParsedFunction -> Context
insertContext c (Left m)  = c { markups = m : (markups c) }
insertContext c (Right p) = case p of
                              ParsedFunction True  f -> c { plotFunctions = f : (plotFunctions c), functions = f : (functions c) }
                              ParsedFunction False f -> c { functions = f : (functions c) }
