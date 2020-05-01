module Day3_v2 where

data Vector = Vector{x::Int, y::Int} deriving Show


-- v1 - v2 = v3
subtractVector:: Vector -> Vector -> Vector
subtractVector (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)

data Slope = Slope{startVec::Vector, endVec::Vector} deriving Show

directionVector:: Slope -> Vector
directionVector (Slope v1 v2) = subtractVector v2 v1


data Wire = Wire{slopes::[Slope]} deriving Show