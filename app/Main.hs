{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Text           as Text
import           Data.Vector         ((!))
import qualified Data.Vector         as V
import           Lib
import           Relude
import Lens.Micro.Platform

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
      Text.unlines $ V.toList $ V.foldr connect (V.fromList $ ["\9547"] <> replicate cellHeight "\9475") $ V.map drawCell vec

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
           else "\9475") <> Text.replicate (topBarLen - 1) " "
       ])
  where
    numLen = Text.length $ show num

connect :: V.Vector Text -> V.Vector Text -> V.Vector Text
connect v1 v2 = mconcat $ fmap (\x -> [(v1 ! x) <> (v2 ! x)]) [0 .. cellHeight - 1]

main :: IO ()
main = putTextLn $ drawGrid g
  where
    g = createGrid 15 15
      & cells . ix 4 . ix 4 . topConnection .~ True
      & cells . ix 3 . ix 5 . leftConnection .~ True
