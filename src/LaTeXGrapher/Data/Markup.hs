module LaTeXGrapher.Data.Markup
    (
      Markup(..)
    , isView
    , isLabel
    ) where

import LaTeXGrapher.Data.Function
import LaTeXGrapher.Data.Expression

data Markup = View [EPoint]
            | Label EPoint String
            | Points [EPoint]
            | Line [EPoint]
            | Segment [EPoint]
            | Secant String Expr Expr
            | Tangent String Expr
            | Shade String Expr Expr
              deriving (Show)

isView :: Markup -> Bool
isView (View _) = True
isView _        = False

isLabel :: Markup -> Bool
isLabel (Label _ _) = True
isLabel _           = False
