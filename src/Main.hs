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
  let v1 = Day3_v2.Vector 2 3
  let v2 = Day3_v2.Vector 6 3

  let v3 = Day3_v2.Vector 0 3
  let v4 = Day3_v2.Vector 4 3
  let slope1 = Slope v1 v2
  let slope2 = Slope v3 v4
  print ( intersection slope1 slope2 )

