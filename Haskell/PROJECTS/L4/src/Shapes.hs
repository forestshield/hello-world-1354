module Shapes   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
, rsMyDtS4       -- this variable is now exported 
) where

--- 
data Point = Point Float Float deriving (Show, Eq)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show, Eq)
-- better surface
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
--
rsMyDtS1 = surface (Rectangle (Point 0 0) (Point 100 100))   -- 10000.0  
rsMyDtS2 = surface (Circle (Point 0 0) 24)                   -- 1809.5574
--
nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
--
rsMyDtS3 = nudge (Circle (Point 34 34) 10) 5 10              -- Circle (Point 39.0 44.0) 10.0
--
baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
--  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- we are exporting this variable to access is in other placess
rsMyDtS4 = nudge (baseRect 40 100) 60 23         -- Rectangle (Point 60.0 23.0) (Point 100.0 123.0)  
