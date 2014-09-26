{-# LANGUAGE DeriveDataTypeable #-}
module Main 
       (
         main
       ) where

import LaTeXGrapher.Math
import LaTeXGrapher.Data.Function
import LaTeXGrapher.Data.Context
import LaTeXGrapher.Data.Markup
import LaTeXGrapher.Parser
import LaTeXGrapher.Parser.Expression
import LaTeXGrapher.Output
import LaTeXGrapher.Plot
import LaTeXGrapher.Repl
import LaTeXGrapher.Diagrams

import System.Console.CmdArgs.Implicit hiding (args,name)

import Data.List
import Data.List.Split
import System.IO
import System.Environment

data GrapherOpts = GrapherOpts
                   { width     :: Int
                   , height    :: Int
                   , output    :: FilePath
                   , input     :: FilePath
                   }
   deriving (Show, Data, Typeable)

grapherOpts :: String -> GrapherOpts
grapherOpts prog = GrapherOpts
  { width =  400
             &= typ "INT"
             &= help "Desired width of the output image (default 400)"

  , height = 400
              &= typ "INT"
              &= help "Desired height of the output image (default 400)"

  , output = def
           &= typFile
           &= help "Output file"
           
  , input = def
           &= argPos 0
  }
  &= summary "Command-line LaTeXGrapher generation."
  &= program prog

stressContext :: GrapherOpts -> Context -> IO ()
stressContext o c = case es of
    Just es -> stressContext' c es
    Nothing -> putStrLn "Eval error in context."
  where 
    fs = plotFunctions c
    ns = map name fs 
    es = sequence . map (lookupF c) $ ns
       
stressContext' c es = putStrLn $ show (sum vs)
  where
    vs = [ e [x] | e <- es, x <- xs ]
    xs = [(fromInteger x) / 100.0 | x <- [-1000..1000]] :: [Double]
             
diagramsContext :: GrapherOpts -> Context -> IO ()
diagramsContext o c = h ps
  where ps = plot c
        mm = minMax c
        h (Left e)    = print e
        h (Right ps') = ppPlot ps' >> plotDiagrams mm ps' (output o) (fromIntegral $ width o) (fromIntegral $ height o)

ppPlot = mapM_ (putStrLn . pp)
  where
    pp p@PlotPoint{} = show p
    pp t@Tick{} = show t
    pp (Plot ss s e l) = concat [ "Plot\n", ppSeg ss, show (s, e, l) ]
    ppSeg (PolyLine ss) = unlines $ map show ss

putStrings :: [String] -> IO ()
putStrings = mapM_ putStrLn

buildFilter :: ([Plot] -> MinMax -> [String]) -> GrapherOpts -> Context -> IO ()
buildFilter f = const (plot >>= h)
  where 
    h (Left e)   _ = print e
    h (Right ps) c = putStrings $ f ps (minMax c)

runProgram o f  = do
  handle <- openFile (input o) ReadMode
  c <- hGetContents handle
  case parseGrapher c of
    Left e  -> putStrLn "Error parsing input: " >> print e
    Right c -> f o c

main = do
  prog <- getProgName
  args <- getArgs
  opts <- cmdArgs (grapherOpts prog)
  chooseContext opts
  
chooseContext :: GrapherOpts -> IO ()
chooseContext opts =
  case splitOn "." (output opts) of
    [""] -> putStrLn "No output file given."
    ["repl"] -> runProgram opts (const replContext)
    ps | last ps `elem` ["eps"] -> do
           let f = case last ps of
                 _     -> diagramsContext
           runProgram opts f
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps