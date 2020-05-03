module Main where
import Day3_v2
import Lib
main :: IO ()
main = do
  contents <- readFile "input/input3_test_1.txt"
  let list = split '\n' contents
  let lines = map (\line -> split ',' line) list

  let commands1 = lines !! 0
  let commands2 = lines !! 1

  let wire1 = Wire [] (Vector 0 0)
  let wire2 = Wire [] (Vector 0 0)

  let last =  (LineSegment (Vector 3 2) (Vector 3 5))
  let last2 =  (LineSegment (Vector 2 3) (Vector 6 3))

  let wire1_ = createAddLineSegments wire1 ["R8","U5","L5","D3"]
  let wire2_ = createAddLineSegments wire2 ["U7","R6","D4","L4"]

  let ls2 = slopes wire2_
  print(calculateTU last last2)

