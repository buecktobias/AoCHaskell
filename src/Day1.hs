module Day1 where

fuelNeeded:: Int -> Int
fuelNeeded mass = floor ((fromIntegral mass) / 3) - 2
