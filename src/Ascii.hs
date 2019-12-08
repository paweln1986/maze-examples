{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Ascii where

import           Common
import qualified Data.Text   as Text
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Relude

cellWidth :: Int
cellWidth = 5

cellHeight :: Int
cellHeight = 3

topBar :: Text
topBar = "\9547" <> Text.replicate cellWidth "\9473"

emptyTopBar :: Text
emptyTopBar = "\9547" <> Text.replicate cellWidth " "

endingBar :: Text
endingBar = Text.reverse topBar

topBarLen :: Int
topBarLen = Text.length topBar

drawGrid :: Grid -> Text
drawGrid (Grid _ cellsToRender cw _) =
  foldl' (\x y -> x <> drawLine y) "" cellsToRender <> "\9547" <> Text.replicate cw endingBar
  where
    drawLine vec =
      Text.unlines $
      V.toList $ V.foldr connect (V.fromList $ ["\9547"] <> replicate cellHeight "\9475") $ V.map drawCell vec

drawCell :: Cell -> V.Vector Text
drawCell (Cell _ isConnectedFromTop isConnectedFromLeft _ _) =
  [ if isConnectedFromTop
      then emptyTopBar
      else topBar
  ] <>
  mconcat
    (replicate
       cellHeight
       [ (if isConnectedFromLeft
            then " "
            else "\9475") <>
         Text.replicate (topBarLen - 1) " "
       ])

connect :: V.Vector Text -> V.Vector Text -> V.Vector Text
connect v1 v2 = mconcat $ fmap (\x -> [(v1 ! x) <> (v2 ! x)]) [0 .. cellHeight - 1]

showMaze :: Int -> Int -> (Int -> Int -> Grid -> IO Grid) -> IO ()
showMaze x y gridGenerator = do
  grid <- gridGenerator 0 0 g
  putTextLn $ drawGrid grid
  where
    g = createGrid x y
