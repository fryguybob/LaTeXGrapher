module LaTeXGrapher.Repl 
    (
      repl
    , replContext
    ) where

import Control.Monad

import LaTeXGrapher.Data.Context
import LaTeXGrapher.Data.Function(Function(Function),FunctionData(DefinedFunction))
import LaTeXGrapher.Parser
import LaTeXGrapher.Parser.Expression

import Text.ParserCombinators.Parsec
import System.IO
import Data.List(sortBy,find,isPrefixOf)

putStrFlush :: String -> IO ()
putStrFlush s = do putStr s
                   hFlush stdout

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse exprEol "expression" (s ++ "\n")

repl :: String -> IO ()
repl s = case parseGrapher s of
              Left e  -> putStrLn (show e)
              Right c -> replContext c

replContext :: Context -> IO ()
replContext c = do putStrLn "Context loaded. Enter \":q\" to quit."
                   replC c

data Command = Command { name        :: String
                       , description :: String
                       , action      :: Context -> IO Bool
                       }
        
showHelp :: [Command] -> IO ()          
showHelp = mapM_ h
    where h (Command _ [] _) = return ()
          h (Command n d  _) = putStrLn $ n ++ " -- " ++ d

definedFunctions :: Context -> [Function]
definedFunctions c = filter h $ functions c
    where h (Function _ (DefinedFunction _)) = False
          h _                                = True

showFunctions f = mapM_ (putStrLn . show) (definedFunctions f)

commands :: [Command]
commands = sortBy (\a b -> compare (name a) (name b))
             [ Command ":quit" "Closes the program." 
                $ \_ -> return False
             , Command ":help" "Shows the help." 
                $ \_ -> showHelp commands >> return True
             , Command ":browse" "Shows the functions in the current context." 
                $ \c -> showFunctions c >> return True
             , Command ":~" ""
                $ \_ -> showHelp commands >> return True
             ]

replC :: Context -> IO ()
replC c = do putStrFlush "> "
             l <- getLine
             runOrEval l $ find (isPrefixOf l . name) commands
    where run a = (action a) c >>= (`when` (replC c))
          runOrEval _ (Just a) = run a
          runOrEval l Nothing  = eval l >> replC c
          eval s = case parseExpr s of
                        Left  e -> putStrLn (show e)
                        Right e -> showEval (evaluateExpr c e)
          showEval Nothing  = putStrLn "Eval error."
          showEval (Just e) = putStrLn $ "= " ++ (show e)
