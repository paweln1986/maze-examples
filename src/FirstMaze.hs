{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module FirstMaze where

import qualified Data.Text           as Text
import           Data.Vector         ((!))
import qualified Data.Vector         as V
import           Lens.Micro.Platform
import           Lib
import           Relude              hiding (Down, Left, Right, Top)
import           System.Random       (Random (..), randomIO)
import Control.Concurrent (threadDelay)

data Directions
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Enum, Bounded)

randomDirection :: IO Directions
randomDirection = toEnum <$> randomRIO (fromEnum $ minBound @Directions, fromEnum $ maxBound @Directions)

data Cell =
  Cell
    { _cellId         :: Int
    , _topConnection  :: Bool
    , _leftConnection :: Bool
    }
  deriving (Show)

makeLenses ''Cell

cellWidth = 5

cellHeight = 3

data Grid =
  Grid
    { _name   :: Text
    , _cells  :: V.Vector (V.Vector Cell)
    , _width  :: Int
    , _height :: Int
    }

makeLenses ''Grid

createGrid :: Int -> Int -> Grid
createGrid x y = Grid "" (V.fromList [V.fromList ((\x -> Cell (x * i) False False) <$> [1 .. x]) | i <- [1 .. y]]) x y

topBar = "\9547" <> Text.replicate cellWidth "\9473"

emptyTopBar = "\9547" <> Text.replicate cellWidth " "

endingBar = Text.reverse topBar

topBarLen = Text.length topBar

drawGrid :: Grid -> Text
drawGrid (Grid _ cells w h) = foldl' (\x y -> x <> drawLine y) "" cells <> "\9547" <> Text.replicate w endingBar
  where
    drawLine vec =
      Text.unlines $ V.toList $ V.foldr connect (V.fromList $ ["\9547"] <> replicate cellHeight "\9475") $
      V.map drawCell vec

drawCell :: Cell -> V.Vector Text
drawCell (Cell num topConnection leftConnection) =
  [ if topConnection
      then emptyTopBar
      else topBar
  ] <>
  mconcat
    (replicate
       cellHeight
       [ (if leftConnection
            then " "
            else "\9475") <>
         Text.replicate (topBarLen - 1) " "
       ])
  where
    numLen = Text.length $ show num

connect :: V.Vector Text -> V.Vector Text -> V.Vector Text
connect v1 v2 = mconcat $ fmap (\x -> [(v1 ! x) <> (v2 ! x)]) [0 .. cellHeight - 1]

showMaze :: IO ()
showMaze = do
  grid <- doMaze 15 15 0 0 g
  putTextLn $ drawGrid grid
  where
    g = createGrid 15 15

calculateDirection :: Int -> Int -> IO Directions
calculateDirection x y = do
  move <- randomDirection
  case move of
    Up
      | y <= 1 -> calculateDirection x y
    Left
      | x <= 1 -> calculateDirection x y
    _ -> return move

doMaze :: Int -> Int -> Int -> Int -> Grid -> IO Grid
doMaze maxX maxY x y grid =
  if x < maxY
    then do
      connect :: Bool <- randomIO
      let nextX = if y < maxX then x else x + 1
      let nextY = if y < maxX then y+1 else 0
      if connect
        then doMaze maxX maxY nextX nextY (grid & cells . ix x . ix (y + 1) . leftConnection .~ True)
        else doMaze maxX maxY nextX nextY (grid & cells . ix (x + 1) . ix y . topConnection .~ True)
    else return grid
