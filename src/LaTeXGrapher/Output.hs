module LaTeXGrapher.Output
    (
      postscriptFilter,
    ) where

import LaTeXGrapher.Math
import LaTeXGrapher.Plot

import Data.VectorSpace

import Data.List(intersperse)

postscriptFilter :: [Plot] -> MinMax -> [String]
postscriptFilter ds mm = header width height : (concat $ map (toPS $ tm mm) ds)
  where tm mm p = (toPt' p) ^+^ (toPt' (w / 2, h / 2))
        toPt = (*) (dpi * m' / m)
        toPt' (x,y) = (toPt x, toPt y)
        m = max w h
        m' = max width height
        w = mmWidth mm
        h = mmHeight mm

toPS :: (Point -> Point) -> Plot -> [String]
toPS tm (Plot g s e _) = geometry tm g
toPS tm (PlotPoint p)  = [circle (tm p) pointRadius]

geometry tm (PolyLine ps) = gLines tm ps

gLines _ []      = []
gLines tm (x:xs) = newpath : (moveto $ tm x) : (h xs)
   where h [] = [stroke]
         h (x:xs) = (lineto $ tm x) : (h xs)

point (x,y) = (show x) ++ " " ++ (show y)

moveto a = (point a) ++ " moveto"
lineto a = (point a) ++ " lineto"
circle a r = newpath ++ " " ++ (point a) ++ " " ++ (show r) ++ " 0 360 arc closepath fill"
newpath = "newpath"
stroke = "stroke"

-- -------------------
-- Sizes (these could end up in a "style" or "configuration" file in the future).
-- in points
pointRadius = 2.5
dpi = 72        -- points per inch

-- inches
width = 3       
height = 3      -- inches

header w h = "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 0 0 " ++ (show $ dpi * w) ++ " " ++ (show $ dpi * h) ++ "\n% Created with LaTeXGrapher."