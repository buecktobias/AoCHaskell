module Lib where

updateListElement:: [a] -> Int -> a -> [a]
updateListElement list index newElement = xs ++ [newElement] ++ (tail ys)
                                          where 
                                          (xs, ys) = splitAt index list

readInt :: String -> Int
readInt = read

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs
