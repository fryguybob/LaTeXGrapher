module LaTeXGrapher.Parser.Expression
    (
      evaluate
    , evaluateConditional
    , Expr(..)
    , expr
    , exprList
    , exprEol
    , EvalError(..)
    ) where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import LaTeXGrapher.Data.Context
import LaTeXGrapher.Data.Expression
import LaTeXGrapher.Math
import LaTeXGrapher.Math.Bool
import LaTeXGrapher.Parser.WhiteSpace

data EvalError = EvalParseError ParseError | EvalError String deriving (Show)

evaluate :: String -> Either EvalError Double
evaluate e = evaluateConst $ parse expressionGrammer "Expression" e

evaluateConditional :: String -> Either EvalError Bool
evaluateConditional e = toEitherBool $ evaluate e

expressionGrammer = expr

evaluateConst :: Either ParseError Expr -> Either EvalError Double
evaluateConst (Left e)  = Left . EvalParseError $ e
evaluateConst (Right e) = case evaluateExpr defaultContext e of
                            Just a  -> Right a
                            Nothing -> Left . EvalError $ "Unbound name"

-- Addapted from Parsec manual.

expressionDef :: LanguageDef st
expressionDef = emptyDef
             { identStart	 = letter
             , identLetter	 = alphaNum <|> oneOf "_'"
             , opStart       = oneOf "<=|&!*+-^/>"
             , opLetter      = oneOf "=|&"
             }

isSpaceNoLine :: Char -> Bool
isSpaceNoLine ' '  = True
isSpaceNoLine '\t' = True
isSpaceNoLine _    = False

lexer :: P.TokenParser ()
lexer = makeTokenParserWithCustomWhiteSpace isSpaceNoLine expressionDef
  
whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

toDouble :: Either Integer Double -> Double
toDouble (Left n)  = fromIntegral n
toDouble (Right d) = d

float :: Parser Expr
float = liftM (Const . toDouble) (P.naturalOrFloat lexer)

expr :: Parser Expr
expr = buildExpressionParser table factor
        <?> "expression"

table   = [ [uopf "!"]
          , [opf "^" AssocRight]
          , [uop "-" (fromUnary "neg")]
          , [opf "*" AssocLeft, opf "/" AssocLeft]
          , [opf "+" AssocLeft, opf "-" AssocLeft]
          , [opf "&" AssocLeft, opf "||" AssocLeft, opf "&&" AssocLeft]
          , [opf ">=" AssocLeft, opf "<=" AssocLeft, opf ">" AssocLeft, opf "<" AssocLeft, opf "==" AssocLeft, opf "!=" AssocLeft]
          ]
        where
          opf s assoc
             = op s (fromBinary s) assoc
          op s f assoc
             = Infix (do{ reservedOp s; return f} <?> "infix operator") assoc
          uopf s
             = uop s (fromUnary s)
          uop s f
             = Prefix (do { reservedOp s; return f} <?> "prefix operator")

exprList :: Parser [Expr]
exprList = sepBy expr (symbol ",") <?> "expression list"

exprEol :: Parser Expr
exprEol = do e <- expr
             eol
             return e

eol = (char '\n')


functionCall :: Parser Expr
functionCall = do n <- identifier
                  args <- option [] (parens exprList)
                  return $ ExprFunc n args

factor  =   functionCall
        <|> parens expr
        <|> float
        <?> "simple expression"