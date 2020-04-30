module Main where
import Day1
import Day2
import Lib

main :: IO ()
main = do
  contents <- readFile "input/input2.txt"
  let list = split ',' contents
  let intList = map Lib.readInt list
  let restoredProgram = Day2.restoreIntProgram intList
  let testExecuted = Day2.executeIntProgram restoredProgram
  print testExecuted !! 0
