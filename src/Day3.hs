module Day3
  (Wire(..),
  Position(..),
  executeCommand,
  moveUp)
  where

data Wire = Wire { positions :: [Position]} deriving (Show)


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
                     } deriving (Show)

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