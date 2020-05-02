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
  let slope1 = Slope v1 v2
  let slope2 = Slope v3 v4
  print (pointsSlope slope1 )

