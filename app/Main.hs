module Main where
import Day3
import Lib

main :: IO ()
main = do
  contents <- readFile "input/input3.txt"
  let list = split '\n' contents
  let lines = map (\line -> split ',' line) list
  print lines
  let pos = Position {x=3, y=3}
  print pos

