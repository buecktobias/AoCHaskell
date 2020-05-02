module Day3_v2 where

data Vector = Vector{x::Int, y::Int} deriving Show


-- v1 - v2 = v3
subtractVector:: Vector -> Vector -> Vector
subtractVector (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)

data Slope = Slope{startVec::Vector, endVec::Vector} deriving Show


calculateTU:: Slope -> Slope -> (Int, Int)
calculateTU (Slope (Vector x1 y1) (Vector x2 y2)) (Slope (Vector x3 y3) (Vector x4 y4))   = (t, u)
                where
                denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
                t = ( ( ( x1 - x3 ) * ( y3 - y4 ) ) - ( ( y1 - y3 ) * ( x3 - x4 ))) `div`  denominator
                u = (( ( x1 -  x2) * (y1 - y3)) - ( ( y1 - y2 ) * (x1 - x3) )) `div` denominator


pointsSlope:: Slope -> [Vector]
pointsSlope slope@(Slope start end) = points slope start

              
points:: Slope -> Vector -> [Vector]
points s1@(Slope _ (Vector endX endY))(Vector x1 y1)
            | new_vec_x == endX && new_vec_y == endY = [new_vec]
            | otherwise = new_vec : (points s1 new_vec)
            where
            dir = directionVector s1
            (Vector xDir yDir) = unitVector dir
            new_vec@(Vector new_vec_x new_vec_y) = Vector (x1 + xDir) (y1 + yDir)
            
            


intersection :: Slope -> Slope -> [Vector]
intersection s1@(Slope v1@(Vector x1 y1) v2@(Vector x2 y2)) s2@(Slope  v3@(Vector x3 y3) v4@(Vector x4 y4))
                                                                                              -- parallel
                                                                                              | denominator == 0 = []
                                                                                              -- not parallel
                                                                                              |otherwise =
                                                                                                           let
                                                                                                           (t, u) = calculateTU s1 s2
                                                                                                           in
                                                                                                           if t >= 0 && t <= 1 then let
                                                                                                                                 intersection_x = x1  + t * (x2 - x1)
                                                                                                                                 intersection_y = y1 + t * (y2 - y1)
                                                                                                                                 in
                                                                                                                                 [Vector intersection_x intersection_y]
                                                                                                            else []

                                                                                              where
                                                                                              denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
                                                                                              s1Dir@(Vector s1DirX s1_dir_y) = directionVector s1
                                                                                              s2_dir = directionVector s2
lengthVector:: Vector -> Int
lengthVector (Vector x1 y1) = x1 + y1


unitVector:: Vector -> Vector
unitVector vec@(Vector x1 y1)  = Vector (x1 `div` l) (y1 `div` l)
                            where
                            l = lengthVector vec


directionVector:: Slope -> Vector
directionVector (Slope v1 v2) = subtractVector v2 v1


data Wire = Wire{slopes::[Slope]} deriving Show