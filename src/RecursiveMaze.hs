{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module RecursiveMaze where

import           Control.Concurrent    (threadDelay)
import qualified Data.Text             as Text
import           Data.Vector           ((!))
import qualified Data.Vector           as V
import           Lens.Micro.Platform
import           Lib
import           Relude                hiding (Down, Left, Right, Top)
import           System.Random         (Random (..), randomIO)
import           System.Random.Shuffle

data Directions
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Enum, Bounded)

instance Random Directions where
  random g =
    case randomR (0, 2) g of
      (r, g') -> (toEnum r, g')
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (r, g') -> (toEnum r, g')

randomDirections :: IO [Directions]
randomDirections = shuffleM [Up .. Right]

randomDirection :: IO Directions
randomDirection = toEnum <$> randomRIO (fromEnum $ minBound @Directions, fromEnum $ maxBound @Directions)

data Cell =
  Cell
    { _cellId         :: Int
    , _topConnection  :: Bool
    , _leftConnection :: Bool
    , _visited        :: Bool
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
createGrid x y =
  Grid "" (V.fromList [V.fromList ((\x -> Cell (x * i) False False False) <$> [1 .. x]) | i <- [1 .. y]]) x y

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
drawCell (Cell num topConnection leftConnection _) =
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

nextMove :: Directions -> Int -> Int -> (Int, Int)
nextMove Up x y    = (x - 1, y )
nextMove Down x y  = (x + 1, y)
nextMove Left x y  = (x, y  - 1)
nextMove Right x y = (x, y + 1)

between y (a, b) = y >= a && y < b

updateGrid x y grid Right = grid & cells . ix x . ix (y+1) . leftConnection .~ True
updateGrid x y grid Left = grid & cells . ix x . ix y . leftConnection .~ True
updateGrid x y grid Down = grid & cells . ix (x+1) . ix y . topConnection .~ True
updateGrid x y grid Up = grid & cells . ix x . ix y . topConnection .~ True

doMaze :: Int -> Int -> Int -> Int -> Grid -> IO Grid
doMaze maxX maxY x y g1 = do
  directions <- randomDirections
  foldlM
    (\grid direction -> do
       let (newX, newY) = nextMove direction x y
       if x `between` (0, maxX) && y `between` (0, maxY) &&
          not (fromMaybe True (grid ^? (cells . ix newX . ix newY . visited)))
         then do

           let g = updateGrid x y grid direction
           doMaze
             maxX
             maxY
             newX
             newY
             (g & (cells . ix x . ix y . visited .~ True) & (cells . ix newX . ix newY . visited .~ True))
         else return grid)
    g1
    directions
