module Main where
import Day1

main :: IO ()
main = do 
        contents <- readFile "input/input1.txt"
        let list =  words contents
        let intList = map readInt list
        let fuelList = map Day1.fuelNeeded intList
        print (sum fuelList)
-- alternately, main = print . map readInt . words =<< readFile "test.txt"

readInt :: String -> Int
readInt = read
