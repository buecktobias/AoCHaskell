module Main where
import Day3
import Lib

main :: IO ()
main = do
  contents <- readFile "input/input3.txt"
  let list = split '\n' contents
  let lines = map (\line -> split ',' line) list
  print lines
  let pos = Position {x=0, y=0}
  print pos
  print( moveUp pos )
  let testWire = Wire [pos]
  let movedWire = executeCommand testWire "R10"
  print movedWire

