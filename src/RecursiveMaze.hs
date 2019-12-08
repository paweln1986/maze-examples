{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RecursiveMaze where

import           Common
import           Lens.Micro.Platform
import           Relude              hiding (Down, Left, Right)

nextMove :: Directions -> Int -> Int -> (Int, Int)
nextMove Up x y    = (x - 1, y)
nextMove Down x y  = (x + 1, y)
nextMove Left x y  = (x, y - 1)
nextMove Right x y = (x, y + 1)

between :: Ord a => a -> (a, a) -> Bool
between y (a, b) = y >= a && y < b

updateGrid :: Int -> Int -> Grid -> Directions -> Grid
updateGrid x y grid Right = grid & cells . ix x . ix (y + 1) . leftConnection .~ True
updateGrid x y grid Left = grid & cells . ix x . ix y . leftConnection .~ True
updateGrid x y grid Down = grid & cells . ix (x + 1) . ix y . topConnection .~ True
updateGrid x y grid Up = grid & cells . ix x . ix y . topConnection .~ True

doMaze :: Int -> Int -> Grid -> (Grid -> IO ()) -> IO Grid
doMaze x y oldGrid onUpdate = do
  let maxX = oldGrid ^. width
  let maxY = oldGrid ^. height
  directions <- randomDirections
  foldlM
    (\gridToProcess direction -> do
       let (newX, newY) = nextMove direction x y
       if x `between` (0, maxX) && y `between` (0, maxY) &&
          not (fromMaybe True (gridToProcess ^? (cells . ix newX . ix newY . visited)))
         then do
           let newGrid = updateGrid x y gridToProcess direction
           let newGrid1 =
                 newGrid & (cells . ix x . ix y . visited .~ True) & (cells . ix newX . ix newY . visited .~ True) &
                 (cells . ix x . ix y . current .~ True) &
                 (cells . ix newX . ix newY . current .~ True)
           onUpdate newGrid1
           a <- doMaze newX newY newGrid1 onUpdate
           let a1 = a & (cells . ix x . ix y . current .~ False) & (cells . ix newX . ix newY . current .~ False)
           onUpdate a1
           return a1
         else return gridToProcess)
    oldGrid
    directions
