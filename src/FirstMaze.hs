{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FirstMaze where

import           Common
import           Lens.Micro.Platform
import           Relude              hiding (Down, Left, Right)
import           System.Random       (randomIO)

doMaze :: Int -> Int -> Int -> Int -> Grid -> (Grid -> IO ()) -> IO Grid
doMaze maxX maxY x y grid onUpdate =
  if x < maxY
    then do
      connect :: Bool <- randomIO
      let nextX =
            if y < maxX
              then x
              else x + 1
      let nextY =
            if y < maxX
              then y + 1
              else 0
      let visitedGrid = grid & cells . ix x . ix y . visited .~ True & cells . ix x . ix y . current .~ True
      onUpdate visitedGrid
      let newGrid =
            if connect
              then visitedGrid & cells . ix x . ix (y + 1) . leftConnection .~ True
              else visitedGrid & cells . ix (x + 1) . ix y . topConnection .~ True
      n <- doMaze maxX maxY nextX nextY newGrid onUpdate
      let updated = n & cells . ix x . ix y . current .~ False
      onUpdate updated
      return updated
    else return grid
