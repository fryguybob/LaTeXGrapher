module LaTeXGrapher.Data.Tabular
    (
      showTabular
    ) where

import Data.List(intersperse, concat)

showTabular :: [[String]] -> String
showTabular rs = concat $ [(showRow cs) ++ "\n" | cs <- rs]
  where ws = colWidths rs
        showRow cs = concat $ map (uncurry showLeft) (zip ws cs)

showTabularWithRowSep :: String -> [[String]] -> String
showTabularWithRowSep sep rs = concat $ [(showRow cs) ++ "\n" | cs <- rs]
  where ws = colWidths rs
        showRow cs = concat $ intersperse sep $ map (uncurry showLeft) (zip ws cs)

showLeft :: Int -> String -> String
showLeft w s | n < 0     = take w s
             | otherwise = concat [s, map (const ' ') [1..n]]
    where n = w - length s

colWidths :: [[[a]]] -> [Int]
colWidths rs = [maximum $ (colWidth n rs) | n <- [0..]]

colWidth :: Int -> [[[a]]] -> [Int]
colWidth n rs = map length (cols n rs)

col :: Int -> [[a]] -> [a]
col 0 xs     = safeHead xs
col n []     = []
col n (x:xs) = col (n-1) xs

cols :: Int -> [[[a]]] -> [[a]]
cols n rs = map (col n) rs

safeHead :: [[a]] -> [a]
safeHead []    = []
safeHead (a:_) = a

-- test = [["hello", "world"],["A greating","a place"],["","world"],["hello",""],[],["","","Third Column!"]]