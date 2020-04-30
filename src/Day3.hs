module Day3
  (Wire(..),
  Position(..),
  executeCommand,
  executeCommands,
  createStartWire,
  uniquePositions,
  wireIntersections,
  minDistIntersections,
  distFromStart,
  moveUp)
  where

import Data.Set (Set, toList)             -- This just imports the type name
import qualified Data.Set as Set
import Data.List (sortOn)

data Wire = Wire { positions :: [Position]} deriving (Show)



distFromStart:: Position -> Int
distFromStart pos = ( abs (x pos) ) + ( abs (y pos) )

uniquePositions:: Wire -> Set Position
uniquePositions wire = Set.fromList (tail (positions wire ) )

minDistIntersections:: [Position] -> Int
minDistIntersections xs = minimum ( map distFromStart xs )

wireIntersections:: Wire -> Wire -> [Position]
wireIntersections wire1 wire2 = intersect
                              where
                              up1 = uniquePositions wire1
                              up2 = uniquePositions wire2
                              intersect = toList (Set.intersection up1 up2)

createStartWire:: Wire
createStartWire = newWire
                  where
                  startPositions = [ Position {x=0, y=0} ]
                  newWire = Wire {positions=startPositions}

executeCommandN:: Wire -> [String] -> Int -> Wire
executeCommandN wire commands n
                                  | n >= (length commands) = wire
                                  | otherwise =
                                  let
                                  command = commands !! n
                                  newWire = executeCommand wire command
                                  in
                                  executeCommandN newWire commands ( n + 1 )

executeCommands:: Wire -> [String] -> Wire
executeCommands wire commands = executeCommandN wire commands 0


executeCommand:: Wire -> String -> Wire
executeCommand wire command = executeN wire amount wireFunc
  where
    direction = head command :: Char
    amount = read (tail command) :: Int
    wireFunc = getWireFunc direction


getWireFunc:: Char -> (Wire -> Wire)
getWireFunc chr
              | chr == 'R' = addRight
              | chr == 'L' = addLeft
              | chr == 'U' = addUp
              | chr == 'D' = addDown

executeN:: Wire -> Int -> (Wire->Wire) -> Wire
executeN wire 0 wireFunc = wire
executeN wire n wireFunc = executeN newWire newN wireFunc
                          where
                          newWire = wireFunc wire
                          newN = n -1


lastPosition:: Wire -> Position
lastPosition wire = last (positions wire)

addUp:: Wire -> Wire
addUp wire = Wire wirePositions
            where
            newPosition = moveUp (lastPosition wire)
            wirePositions = (positions wire) ++ [newPosition]

addDown:: Wire -> Wire
addDown wire = Wire wirePositions
              where
              newPosition = moveDown (lastPosition wire)
              wirePositions = (positions wire) ++ [newPosition]

addLeft:: Wire -> Wire
addLeft wire = Wire wirePositions
               where
               newPosition = moveLeft (lastPosition wire)
               wirePositions = (positions wire) ++ [newPosition]

addRight:: Wire -> Wire
addRight wire = Wire wirePositions
                where
                newPosition = moveRight (lastPosition wire)
                wirePositions = (positions wire) ++ [newPosition]





data Position = Position{ x :: Int,
                         y :: Int
                     } deriving (Show, Eq, Ord)

moveUp:: Position -> Position
moveUp pos = Position newX newY
            where
            newX = x pos
            newY = (y pos) + 1

moveDown:: Position -> Position
moveDown pos = Position newX newY
            where
            newX = x pos
            newY = (y pos)  -1

moveLeft:: Position -> Position
moveLeft pos = Position newX newY
            where
            newX = (x pos) - 1
            newY = y pos

moveRight:: Position -> Position
moveRight pos = Position newX newY
            where
            newX = (x pos) + 1
            newY = y pos