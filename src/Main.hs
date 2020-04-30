module Main where
import Day3
import Lib

import Data.Set (Set)             -- This just imports the type name
import qualified Data.Set as Set
main :: IO ()
main = do
  contents <- readFile "input/input3.txt"
  let list = split '\n' contents
  let lines = map (\line -> split ',' line) list
  let commands1 = lines !! 0
  let commands2 = lines !! 1
  let wire1 = executeCommands createStartWire commands1
  let wire2 = executeCommands createStartWire commands2
  let intersections = wireIntersections wire1 wire2
  print intersections
  print (minDistIntersections intersections)

