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
                                                  | otherwise = 
                                                    let 
                                                    optCodeFunc = getOptCodeFunction optCode
                                                    newIntProgram = executeOptCode optCodeFunc position intProgram 
                                                    newPosition = position + 4
                                                    in 
                                                    executeIntProgramAtPosition newIntProgram newPosition  
                                                    
                                                    where
                                                    optCode = intProgram !! position
                                                    
executeIntProgram:: [Int] -> [Int]
executeIntProgram intProgram = executeIntProgramAtPosition intProgram startPosition
                              where
                              startPosition = 0                                                    

restoreIntProgram:: [Int] -> [Int]
restoreIntProgram intProgram = newIntProgram
                              where 
                              newIntProgram = Lib.updateListElement (Lib.updateListElement intProgram 1 12) 2 2                                                    