module Main where
import Day1
import Lib

main :: IO ()
main = do 
        contents <- readFile "input/input2.txt"
        let list =  split ',' contents
        -- let intList = map Lib.readInt list
        let intList = [1,2,3,4]
        let updatedIntList = Lib.updateListElement intList 0 5
        print updatedIntList


