{-# LANGUAGE TupleSections #-}
module LaTeXGrapher.Plot
    (
      plot
    , Plot(..)
    , EndMark(..)
    , Segments(..)
    , LineStyle(..)
    , light
    , dark
    , clip
    , isEmpty
    ) where

import Data.Maybe(maybeToList)
import Data.List(find,span)

import Control.Applicative
import Control.Arrow
import Control.Monad.Instances

import Data.VectorSpace

import LaTeXGrapher.Math
import LaTeXGrapher.Data.Context
import LaTeXGrapher.Data.Markup
import LaTeXGrapher.Data.Function
import LaTeXGrapher.Parser.Expression

data LineStyle = Light | Dark deriving (Show)
data EndMark = Arrow | Closed | Open | None deriving (Show)
data Plot = Plot { segments  :: Segments
                 , startMark :: EndMark
                 , endMark   :: EndMark
                 , lineStyle :: LineStyle
                 }
          | PlotPoint Point
          | Tick Point Point
          deriving (Show)
data Segments = PolyLine [Point] deriving (Show)

plot :: Context -> Either EvalError [Plot]
plot c = plot' c <$> (fmap concat . sequence $ 
    (apMap c plotFunction plotFunctions ++ apMap c plotMarkup markups))

plot' c ms = (axis $ minMax c) ++ ms

apMap c f g = map (f c) (g c)

isEmpty :: Plot -> Bool
isEmpty (Plot (PolyLine []) _ _ _) = True
isEmpty _ = False

light :: Plot -> Plot
light p = p { lineStyle = Light }

dark :: Plot -> Plot
dark p = p { lineStyle = Dark }

axis :: MinMax -> [Plot]
axis mm = concat $ f <$> [(0,0)] <*> [(1,0),(0,1)]
  where f a = g . clip mm . (a:) . (:[])
        g ps = (light . arrows . PolyLine $ ps) : ticks ps

ticks :: [Point] -> [Plot]
ticks (a@(ax,ay):b@(bx,by):_) = [ Tick v p | p <- ps ]
  where v = perp . normalized $ b ^-^ a
        ps = map (fromIntegral *** fromIntegral) . filter (/= (0,0)) $ is
        is | abs (ax - bx) < abs (ay - by) = zip (cycle [floor ax]) (ay...by) 
           | otherwise                     = zip (ax...bx) (cycle [floor ay])
        a ... b = [ceiling (0.1 + min a b)..floor (max a b - 0.1)]
ticks _ = []

perp (x,y) = (-y,x)

totalSteps = 100

plotFunction :: Context -> Function -> Either EvalError [Plot]
plotFunction c f = maybe 
  (Left . EvalError $ "undefined function")
  (Right . uncurry (plotFunction' c))
  (evaluatePlotFunction c f)

plotFunction' :: Context -> [Double] -> (Double -> Double) -> [Plot]
plotFunction' c ss f = (map dark plots) ++ ss'
  where
    plots = concatMap (clipPlot mm) pss
    pss = ranges Arrow xs qs
    xs = [(x, f x) | n <- [l..u], x <- [n / step]]
    step = totalSteps / (mmWidth mm)
    mm = minMax c
    l  = step * (fst $ mmMin mm)
    u  = step * (fst $ mmMax mm)
    qs = map (id &&& querySpecial (mmWidth mm / (1000*totalSteps)) f) $ ss
    ss' = map PlotPoint . filter (mm `mmContains`) . map ((,) <*> f) . map fst $ qs

data SpecialEnd  = SOpen | SClosed deriving (Eq, Show)
data SpecialMark = SLine | SPoint deriving (Eq, Show)
data Special = Special Double SpecialEnd SpecialMark SpecialEnd Double deriving (Eq, Show)

querySpecial :: Double -> (Double -> Double) -> Double -> Special
querySpecial e f x = Special m a mark b p
  where
    a = diff m y
    b = diff y p

    m = f (x - e)
    y = f x
    p = f (x + e)

    mark | abs (m - p) > 4 * e = SPoint
         | otherwise           = SLine

    diff a b | abs (a - b) < 2 * e = SClosed
             | otherwise           = SOpen

ranges :: EndMark -> [Point] -> [(Double,Special)] -> [Plot]
ranges _  [] _          = []
ranges em ps []         = [Plot (PolyLine ps) em Arrow Dark]
ranges em ps ((a,m):ss) = as : ranges em' bs ss
  where
    (as,(em',bs)) = f m
    f (Special sy s _ e ey) = second ((mark e,) . h ey e) 
                            . first (p s . reverse . h sy s . reverse)
                            . span ((< a) . fst) $ ps
    h y SOpen   = replace (a,y)
    h y SClosed = replace (a,y)
    p sy ps = Plot (PolyLine ps) em (mark sy) Dark
    mark SOpen   = Open
    mark SClosed = Closed
    

replace :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
replace p [] = [p]
replace p@(a,_) ts@((b,_):ps) 
  | a == b    = p:ps
  | otherwise = p:ts

clipPlot :: MinMax -> Plot -> [Plot]
clipPlot mm (Plot (PolyLine ps) s e l) = f . split (mm `mmContains`) $ ps
  where
    f [] = []
    f (ps:[]) = [Plot (PolyLine ps) s e l]
    f (ps:pss) = Plot (PolyLine ps) s Arrow l : h pss
    h [] = []
    h (ps:[]) = [Plot (PolyLine ps) Arrow e l]
    h (ps:pps) = Plot (PolyLine ps) Arrow Arrow l : h pps
clipPlot mm a@(PlotPoint p) 
  | mm `mmContains` p = [a]
  | otherwise         = []

split :: (a -> Bool) -> [a] -> [[a]]
split f as = step as
  where step [] = []
        step vs = let (hs,ts) = span f vs in
                  hs : (step $ dropWhile (not . f) ts)


plotMarkup :: Context -> Markup -> Either EvalError [Plot]
plotMarkup c m = f <$> points c ps
  where
    (f, ps) = plotMarkup' c m
    points c = maybe (Left . EvalError $ "unbound markup") Right . sequence . map (evaluateEPoint c)

plotMarkup' c (Points ps)    = (map PlotPoint, ps)
plotMarkup' c (Segment ps)   = ((:[]) . nones . PolyLine, ps)
plotMarkup' c (Line ps)      = ((:[]) . nones . PolyLine . clip (minMax c), ps)
plotMarkup' c (Secant f a b) = (const [], [])
plotMarkup' c (Tangent f x)  = (const [], [])
plotMarkup' c (Shade f a b)  = (const [], [])
plotMarkup' _ _ = (const [], [])

clip :: MinMax -> [Point] -> [Point]
clip mm (a:b:[]) = concat $ map snd $ maybeToList $ find fst options
  where options = [(horz, horzLn), (vert, vertLn), (onTwo, ln)]

        l = mmMin mm
        u = mmMax mm
        v = b ^-^ a
        mm' = mm `mmSub` a
        l' = l ^-^ a
        u' = u ^-^ a
        
        horz = snd v == 0
        horzLn = spany l u (snd a)

        vert = fst v == 0
        vertLn = spanx l u (fst a)

        onTwo = len == 2 || len == 4
        ln = head crossings' : [head $ tail crossings']
        
        len = length crossings
        crossings = filter (mm' `mmContains`) [fx' (fst l'), fx' (fst u'), fy' (snd l'), fy' (snd u')]
        crossings' = map (^+^ a) $ take 2 crossings

        m = (snd v) / (fst v)
        fx x = m * x
        fy y = y / m
        fx' x = (x, fx x)
        fy' y = (fy y, y)


spany (lx,ly) (ux,uy) y | inOpenInterval y ly uy = [(lx,y),(ux,y)]
                        | otherwise              = []

spanx (lx,ly) (ux,uy) x | inOpenInterval x lx ux = [(x,ly),(x,uy)]
                        | otherwise              = []

arrows :: Segments -> Plot
arrows s = Plot s Arrow Arrow Light

nones :: Segments -> Plot
nones s = Plot s None None Light