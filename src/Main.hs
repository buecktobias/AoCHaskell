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
  let v1 = Day3_v2.Vector{x=3,y=3}
  let v2 = Day3_v2.Vector{x=6,y=3}
  print v2
  print (subtractVector v2 v1)

