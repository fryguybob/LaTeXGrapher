module LaTeXGrapher.Parser.Markup
    (
      parseMarkup
    , listOfPairs
    ) where

import Data.Map
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import LaTeXGrapher.Data.Context
import LaTeXGrapher.Data.Expression
import LaTeXGrapher.Data.Markup
import LaTeXGrapher.Data.Function
import LaTeXGrapher.Parser.Grammer
import LaTeXGrapher.Parser.Expression

parseMarkup :: Parser Markup
parseMarkup = parseView <|> parseLabel <|> parsePoints <|> parseLine <|> parseSegment <|> parseSecant <|> parseTangent <|> parseShade

parseView :: Parser Markup
parseView = do try $ symbol "View"
               p <- listOfPairs
               return $ View p

parseLabel :: Parser Markup
parseLabel = do try $ symbol "Label"
                p <- parenPair
                s <- toEndOfLine
                return $ Label p s 

parsePoints :: Parser Markup
parsePoints = do try $ symbol "Point"
                 p <- listOfPairs
                 return $ Points p

parseLine :: Parser Markup
parseLine = do try $ symbol "Line"
               p <- listOfPairs
               return $ Line p

parseSegment :: Parser Markup
parseSegment = do try $ symbol "Segment"
                  p <- listOfPairs
                  return $ Segment p

parseSecant :: Parser Markup
parseSecant = do try $ symbol "Secant"
                 n <- identifier
                 symbol ","
                 p <- pair
                 return $ Secant n (fst p) (snd p)

parseTangent :: Parser Markup
parseTangent = do try $ symbol "Tangent"
                  n <- identifier
                  symbol ","
                  e <- expr
                  return $ Tangent n e

parseShade :: Parser Markup
parseShade = do try $ symbol "Shade"
                n <- identifier
                symbol ","
                p <- pair
                return $ Shade n (fst p) (snd p)

listOfPairs :: Parser [(Expr,Expr)]
listOfPairs = sepBy1 parenPair (char ',') <?> "list of pairs"

parenPair :: Parser (Expr,Expr)
parenPair = parens pair

pair :: Parser (Expr,Expr)
pair = do x <- expr
          symbol ","
          y <- expr
          return (x, y) <?> "expression pair"

toEndOfLine :: Parser String
toEndOfLine = manyTill anyChar (char '\n') <?> "string"