{-# LANGUAGE NoImplicitPrelude #-}

module Ui where

import           Common
import qualified Data.Vector       as V
import           Graphics.Gloss
import           Relude
import           Relude.Extra.Lens ((^.))

drawGrid :: Grid -> Picture
drawGrid (Grid _ gridCells _ _) =
  foldr (\(idx, b) a -> a <> translate 0 (fromIntegral $ idx * 50) (drawLine b)) blank (V.indexed gridCells)

drawLine :: V.Vector Cell -> Picture
drawLine gridCells =
  foldr
    (\(idx, b) a -> a <> translate (fromIntegral (idx * 50)) 0 (wasVisited b <> wasSelected b <>box b))
    blank
    (V.indexed gridCells)

wasVisited :: Cell -> Picture
wasVisited cell =
  if cell ^. visited
    then color (greyN 0.79) $ rectangleSolid 50 50
    else blank

wasSelected :: Cell -> Picture
wasSelected cell =
  if cell ^. current
    then color red $ rectangleSolid 50 50
    else blank

line1 :: Float -> Float -> Picture
line1 x y = polygon [(-x / 2, -y / 2), (x / 2, -y / 2), (x / 2, y / 2), (-x / 2, y / 2)]

box :: Cell -> Picture
box cell = color black $ leftLine <> topLine
  where
    leftLine =
      translate (-25) 0 $
      line1 2 50 <>
      if cell ^. leftConnection
        then connectedCell 2 49.5
        else notConnectedCell 2 49.5
    topLine =
      translate 0 (-25) $
      line1 50 2 <>
      if cell ^. topConnection
        then connectedCell 49.5 2
        else notConnectedCell 49.5 2
    connectedCell x y =
      color
        (if cell ^. current
           then red
           else greyN 0.79) $
      line1 x y
    notConnectedCell x y = color (greyN 0.1) (line1 x y)
