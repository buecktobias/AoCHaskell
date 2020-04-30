module Day2 where


addTwoNumbers:: Int -> Int -> Int
addTwoNumbers a b = a + b

executeOptCode:: (a -> b -> c) -> Int -> [Int] -> [Int]
executeOptCode optCodeFunc pos intCode = [2] --optCodeFunc param1 param2
                                        where
                                        param1 = intCode !! (pos+1)
                                        param2 = intCode !! (pos+2)

