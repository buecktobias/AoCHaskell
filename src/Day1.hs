module Day1 where

sumFuel:: [Int] -> Int
sumFuel massList= sum (map fuelNeeded massList)

sumFuel2:: [Int] -> Int
sumFuel2 massList = sum (map fuelNeeded2 massList)

fuelNeeded:: Int -> Int
fuelNeeded mass = floor ((fromIntegral mass) / 3) - 2


fuelNeeded2:: Int -> Int
fuelNeeded2 mass 
                  | fuel <= 0 = 0
                  | otherwise = fuel + fuelNeeded2 fuel
                  where
                  fuel = fuelNeeded mass

