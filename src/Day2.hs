module Day2 where
import Lib

addTwoNumbers:: Int -> Int -> Int
addTwoNumbers a b = a + b

multiplyTwoNumbers:: Int -> Int -> Int 
multiplyTwoNumbers a b = a * b

haltProgram:: IO()
haltProgram = print "programm finished!"

executeOptCode:: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
executeOptCode optCodeFunc pos intCode = newList
                                        where
                                        param1 = intCode !! (intCode !! (pos + 1))
                                        param2 = intCode !! (intCode !! (pos + 2))
                                        outputPosition = intCode !! (pos + 3)
                                        optCodeResult = optCodeFunc param1 param2
                                        newList = Lib.updateListElement intCode outputPosition optCodeResult 

getOptCodeFunction:: Int -> (Int -> Int -> Int)
getOptCodeFunction optCode 
                          | optCode == 1 = addTwoNumbers
                          | optCode == 2 = multiplyTwoNumbers 
                          | otherwise = error ("wrong optCode !!!" ++ show optCode)
                          
executeIntProgramAtPosition:: [Int] -> Int -> [Int]
executeIntProgramAtPosition intProgram position 
                                                  | optCode == 99 = intProgram
                                                  | optCode == 1 || optCode == 2  = 
                                                    let 
                                                    optCodeFunc = getOptCodeFunction optCode
                                                    newIntProgram = executeOptCode optCodeFunc position intProgram 
                                                    newPosition = position + 4
                                                    in 
                                                    
                                                    executeIntProgramAtPosition newIntProgram newPosition  
                                                    
                                                  | otherwise = intProgram
                                                  
                                                    where
                                                    optCode = intProgram !! position


resultIntProgramNounVerb:: [Int] -> Int -> Int -> Int
resultIntProgramNounVerb intProgram noun verb = result
                                                where
                                                restoredIntProgram = setNounAndVerb intProgram noun verb
                                                result = resultIntProgram restoredIntProgram


resultIntProgram:: [Int] -> Int
resultIntProgram intProgram = head (executeIntProgram intProgram)
                                                    
executeIntProgram:: [Int] -> [Int]
executeIntProgram intProgram = executeIntProgramAtPosition intProgram startPosition
                              where
                              startPosition = 0                                                    

restoreIntProgram:: [Int] -> [Int]
restoreIntProgram intProgram = newIntProgram
                              where 
                              newIntProgram = setNounAndVerb intProgram 12 2      

setNounAndVerb:: [Int] -> Int -> Int -> [Int]
setNounAndVerb intProgram noun verb = newIntProgram
                                    where 
                                    newIntProgram = Lib.updateListElement (Lib.updateListElement intProgram 1 noun) 2 verb


bruteForceIntProgram:: [Int] -> Int -> (Int, Int)
bruteForceIntProgram intProgram expectedResult =  head results
                                                where
                                                xs = [0 .. 100]
                                                ys = [0 .. 100]
                                                all_ys = take (100*100) (cycle ys)
                                                all_xs = concat [replicate 100 x | x <- xs]
                                                all_combinations = zip all_xs all_ys
                                                results = filter (\(x,y) -> ( resultIntProgramNounVerb intProgram x y == expectedResult )) all_combinations                            