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
  let v1 = Day3_v2.Vector 3 3
  let v2 = Day3_v2.Vector 6 3
  let slope = Slope v1 v2
  print slope
  print (directionVector slope)

