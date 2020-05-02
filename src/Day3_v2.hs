module Day3_v2 where

data Vector = Vector{x::Int, y::Int} deriving Show


-- v1 - v2 = v3
subtractVector:: Vector -> Vector -> Vector
subtractVector (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)

data Slope = Slope{startVec::Vector, endVec::Vector} deriving Show


intersection :: Slope -> Slope -> Vector
intersection (Slope (Vector x1 y1) (Vector x2 y2)) (Slope  (Vector x3 y3) (Vector x4 y4)) 
                                                                                              | u >= 0 && u <= 1 = let 
                                                                                                                    intersection_x = x3  + u * (x4 - x3)
                                                                                                                    intersection_y = y3 + u * (y4 - y3)
                                                                                                                    in 
                                                                                                                    Vector intersection_x intersection_y
                                                                                              | t >= 0 && t <= 1 = let 
                                                                                                                    intersection_x = x1  + t * (x2 - x1)
                                                                                                                    intersection_y = y1 + t * (y2 - y1)      
                                                                                                                    in
                                                                                                                    Vector intersection_x intersection_y
                                                                                              | otherwise = Vector  0 0                                 
                                                                                              where
                                                                                              denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4) 
                                                                                              t = ( ( ( x1 - x3 ) * ( y3 - y4 ) ) - ( ( y1 - y3 ) * ( x3 - x4 ))) `div`  denominator  
                                                                                              u = (( ( x1 -  x2) * (y1 - y3)) - ( ( y1 - y2 ) * (x1 - x3) )) `div` denominator              

directionVector:: Slope -> Vector
directionVector (Slope v1 v2) = subtractVector v2 v1


data Wire = Wire{slopes::[Slope]} deriving Show