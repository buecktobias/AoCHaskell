module Main where
import Day1

main :: IO ()
main = do 
        contents <- readFile "input/input1.txt"
        let list =  words contents
        let intList = map readInt list
        let result = Day1.sumFuel2 intList
        print (result)

readInt :: String -> Int
readInt = read
