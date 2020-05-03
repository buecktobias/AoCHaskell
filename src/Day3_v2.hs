module Day3_v2 where

import Data.Set
data Vector = Vector{x::Int, y::Int} deriving (Show, Eq, Ord)


distanceNearestVector:: [Vector] -> Int
distanceNearestVector vs = vsLength !! 1
                          where
                          vsLength = Prelude.map lengthVector vs

intersectionPointsSlope:: LineSegment -> LineSegment -> [Vector]
intersectionPointsSlope s1 s2 = toList intersecting_points
                    where
                    points1 = fromList (pointsSlope s1)
                    points2 = fromList (pointsSlope s2)
                    intersecting_points = intersection points1 points2


-- v1 - v2 = v3
subtractVector:: Vector -> Vector -> Vector
subtractVector (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)

data LineSegment = LineSegment{startVec::Vector, endVec::Vector} deriving (Show, Eq, Ord)


calculateTU:: LineSegment -> LineSegment -> (Float, Float)
calculateTU (LineSegment (Vector x1 y1) (Vector x2 y2)) (LineSegment (Vector x3 y3) (Vector x4 y4)) = (t, u)
  where
    denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    t_1 = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
    t = fromIntegral t_1 / fromIntegral denominator
    u = 0.0


pointsSlope:: LineSegment -> [Vector]
pointsSlope slope@(LineSegment start end) = points slope start


points:: LineSegment -> Vector -> [Vector]
points s1@(LineSegment _ (Vector endX endY)) v@(Vector x1 y1)
            | x1 == endX && y1 == endY = [v]
            | otherwise = v : (points s1 new_vec)
            where
            dir = directionVector s1
            (Vector xDir yDir) = unitVector dir
            new_vec@(Vector new_vec_x new_vec_y) = Vector (x1 + xDir) (y1 + yDir)




intersectionSlopes :: LineSegment -> LineSegment -> [Vector]
intersectionSlopes s1@(LineSegment v1@(Vector x1 y1) v2@(Vector x2 y2)) s2@(LineSegment v3@(Vector x3 y3) v4@(Vector x4 y4))
                                                                                              -- parallel
  | denominator == 0 = intersectionPointsSlope s1 s2
                                                                                              -- not parallel
  | otherwise =
    let (t, u) = calculateTU s1 s2
     in if t >= 0 && t <= 1
          then let intersection_x = round (fromIntegral x1 + t * (fromIntegral (x2 - x1)))
                   intersection_y = round (fromIntegral y1 + t * (fromIntegral (y2 - y1)))
                in [Vector intersection_x intersection_y]
          else []
  where
    denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    s1Dir@(Vector s1DirX s1_dir_y) = directionVector s1
    s2_dir = directionVector s2
lengthVector:: Vector -> Int
lengthVector (Vector x1 y1) =( abs x1 ) + (abs y1)


unitVector:: Vector -> Vector
unitVector vec@(Vector x1 y1)  = Vector (x1 `div` l) (y1 `div` l)
                            where
                            l = lengthVector vec


directionVector:: LineSegment -> Vector
directionVector (LineSegment v1 v2) = subtractVector v2 v1


data Wire = Wire{slopes:: [LineSegment], lastPoint:: Vector} deriving Show


addLineSegment:: Wire -> LineSegment -> Wire
addLineSegment (Wire lss _) ls@(LineSegment _ e) = Wire (lss ++ [ls]) e


getDirVec:: Char -> Vector
getDirVec chr
              | chr == 'R' = Vector 1 0
              | chr == 'L' = Vector (-1) 0
              | chr == 'U' = Vector 0 1
              | chr == 'D' = Vector 0 (-1)



createLineSegment:: Wire -> String -> LineSegment
createLineSegment (Wire _ lp) s = LineSegment startPoint endPoint
                                  where
                                  direction = head s :: Char
                                  len = read (tail s) :: Int
                                  (Vector xDir yDir) = getDirVec direction
                                  startPoint@(Vector startX startY) = lp
                                  endPoint = Vector (startX + len * xDir) (startY + len * yDir)

createAddLineSegment:: Wire -> String -> Wire
createAddLineSegment w s = addLineSegment w ls
                          where
                          ls = createLineSegment w s

createAddLineSegments::Wire -> [String] -> Wire
createAddLineSegments w [s] = createAddLineSegment w s
createAddLineSegments w (s:ss) = createAddLineSegments new_wire ss
                            where
                            new_wire = createAddLineSegment w s

intersectionsLineSegmentLineSegments:: LineSegment -> [LineSegment] -> [Vector]
intersectionsLineSegmentLineSegments s1 [s2] = intersectionSlopes s1 s2
intersectionsLineSegmentLineSegments s1 (s2:lineSegments) = intersectionSlopes s1 s2 ++ intersectionsLineSegmentLineSegments s1 lineSegments

intersectionBetweenMultipleLineSegmentsAndMultipleLieSegments:: [LineSegment] -> [LineSegment] -> [Vector]
intersectionBetweenMultipleLineSegmentsAndMultipleLieSegments [lineSegment] lineSegments2 = intersectionsLineSegmentLineSegments lineSegment lineSegments2
intersectionBetweenMultipleLineSegmentsAndMultipleLieSegments (lineSegment:lineSegments1) lineSegments2 = vectorIntersections ++ intersectionBetweenMultipleLineSegmentsAndMultipleLieSegments lineSegments1 lineSegments2
                                                                                            where
                                                                                            vectorIntersections = intersectionsLineSegmentLineSegments lineSegment lineSegments2
intersectionsWires:: Wire -> Wire -> [Vector]
intersectionsWires (Wire ss1 _) (Wire ss2 _) =  intersectionBetweenMultipleLineSegmentsAndMultipleLieSegments ss1 ss2
-- toList (fromList (


