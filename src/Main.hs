module Main where
import Day3_v2
import Lib
main :: IO ()
main = do
  contents <- readFile "input/input3.txt"
  let list = split '\n' contents
  let lines = map (\line -> split ',' line) list
  let commands1 = lines !! 0
  let commands2 = lines !! 1
  let v1 = Day3_v2.Vector 0 0
  let v2 = Day3_v2.Vector 3 0

  let v3 = Day3_v2.Vector 1 0
  let v4 = Day3_v2.Vector 5 0
  let ls1 = [LineSegment v1 v2, LineSegment v2  (Vector 3 3), LineSegment (Vector 3 3) (Vector 7 3)]
  let ls2 = [LineSegment (Vector 5 5) (Vector 0 5)]

  print (intersectionBetweenMultipleLineSegmentsAndMultipleLieSegments ls1 ls2 )

