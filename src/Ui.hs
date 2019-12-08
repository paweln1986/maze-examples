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
    (\(idx, b) a -> a <> translate (fromIntegral (idx * 50)) 0 (box b <> wasVisited b <> wasSelected b))
    blank
    (V.indexed gridCells)

wasVisited :: Cell -> Picture
wasVisited cell =
  if cell ^. visited
    then color (greyN 0.79) $ rectangleSolid 49.5 49.5
    else blank

wasSelected :: Cell -> Picture
wasSelected cell =
  if cell ^. current
    then color red $ rectangleSolid 49.5 49.5
    else blank

box :: Cell -> Picture
box cell = color black $ leftLine <> topLine
  where
    leftLine =
      color
        (if cell ^. leftConnection
           then connectedCell
           else notConnectedCell) $
      line [(-25, 25), (-25, -25)]
    topLine =
      color
        (if cell ^. topConnection
           then connectedCell
           else notConnectedCell) $
      line [(-25, -25), (25, -25)]
    connectedCell =
      if cell ^. current
        then red
        else greyN 0.79
    notConnectedCell = greyN 0.1
