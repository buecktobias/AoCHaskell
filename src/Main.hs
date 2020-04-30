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
  let commands = ["R10", "U5"]
  print ( createStartWire )
  let wire2 = executeCommands createStartWire commands
  print( wire2 )

