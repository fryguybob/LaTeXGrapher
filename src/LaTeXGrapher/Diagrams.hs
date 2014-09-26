{-# LANGUAGE TupleSections #-}
module LaTeXGrapher.Diagrams
    (
      plotDiagrams
    ) where

import Diagrams.Prelude
import Diagrams.Segment
import Diagrams.TwoD.Ellipse
import Diagrams.Backend.Postscript

import LaTeXGrapher.Plot
import LaTeXGrapher.Math

import Data.NumInstances
import Data.VectorSpace
import Data.Default

import Data.Foldable

type D = Diagram Postscript R2

plotDiagrams :: MinMax -> [Plot] -> FilePath -> Double -> Double -> IO ()
plotDiagrams mm ps f w h = do
     -- sequence . map print $ ps
     renderDia Postscript (PostscriptOptions f (EPS s)) d
     -- print (size2D d)
  where ps' = filter (not . isEmpty) ps
        d = buildDiagram mm w h ps'
        s = (round w, round h)
  
buildDiagram :: MinMax -> Double -> Double -> [Plot] -> D
buildDiagram mm x y ps = scale s d
  where
    d = foldMap (toDiagram r) ps
    (w,h) = (mmWidth mm, mmHeight mm)
    s = max (x / w) (y / h)
    r = pointRadius / s

toDiagram :: Double -> Plot -> D
toDiagram r (Plot g s e w) = style w (geometry r g s e)
toDiagram r (PlotPoint p)  = circle r # lw 0 # translate p # fc black
toDiagram r (Tick v p)     = (P $ p + v') ~~ (P $ p - v') # style Light
  where v' = v ^* r

geometry r (PolyLine ps) s e = p' # stroke <> marks r s e p
  where
    p  = cubicSpline False (map P ps)
    p' = reversePath . modifySegments (h e) f . reversePath . modifySegments (h s) f $ p
    
    f r s 
      | r <= 0    = Right s
      | r < l     = Right $ adjustSegment s def { adjMethod = ByAbsolute (-r), adjSide = Start }
      | otherwise = Left (r - l)
      where l = arcLength s (r / 10)
    h None   = 0
    h Open   = r
    h Closed = 0
    h Arrow  = r / 4

modifySegments r f (Path ((P p, (Trail (s:ss) c)):ts)) =
    case f r s of
      Left  r' -> let p' = p + segOffset s in
                  modifySegments r' f (Path ((P p', (Trail ss c)):ts))
      Right s' -> let p' = p + (segOffset s - segOffset s') in
                  Path ((P p', (Trail (s':ss) c)):ts)
modifySegments _ _ p = p

marks r s e p = mark r s (startOrientation p) <> mark r e (endOrientation p)

mark r None   (p,v) = mempty
mark r Arrow  (p,v) = arrow # scale r            # rotate (angle v) # translate p
mark r Open   (p,v) = circle r # style Dark      # rotate (angle v) # translate p
mark r Closed (p,v) = circle r # lw 0 # fc black # rotate (angle v) # translate p

angle (x,y) = Rad $ atan2 y x

arrow = fromVertices [P (1,0.5), P (0,0), P (1,-0.5)] # stroke

startOrientation (Path [])        = ((0,0),(0,0))
startOrientation (Path ((P p,t):_)) = (p,) . startVector $ t
  where
    startVector (Trail [] _)                = (0,0)
    startVector (Trail ((Linear v):_) _)    = v
    startVector (Trail ((Cubic a b _):_) _) = b - a

endOrientation = startOrientation . reversePath

style :: LineStyle -> D -> D
style Light = lw 0.5
style Dark  = lw 1.5

pointRadius :: Double
pointRadius = 5