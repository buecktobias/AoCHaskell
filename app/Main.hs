module Main where
import Day1
import Day2
import Lib

main :: IO ()
main = do
  contents <- readFile "input/input2.txt"
  let list = split ',' contents
  let intList = map Lib.readInt list
  -- let result = Day2.resultIntProgramNounVerb intList 12 2
  let nounVerb = bruteForceIntProgram intList 19690720
  let noun = fst nounVerb
  let verb = snd nounVerb
  print (100 * noun + verb)

