module LaTeXGrapher.Parser.Grammer
    (
      grapherDef
    , whiteSpace
    , lexeme
    , symbol
    , parens
    , semi
    , identifier
    , reserved
    , reservedOp
    ) where

import Data.Map
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

grapherDef :: LanguageDef st
grapherDef = emptyDef
             { commentLine	 = "%"
             , identStart	 = letter
             , identLetter	 = alphaNum <|> oneOf "_'"
             , opStart       = oneOf ">=|:"
             , opLetter      = oneOf []
             , reservedNames  = ["View", "Line", "Point", "Label"]
             , reservedOpNames= []
             , caseSensitive  = True
             }

lexer :: P.TokenParser ()
lexer = P.makeTokenParser grapherDef

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer