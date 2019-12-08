{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common
import           Control.Concurrent                  (forkIO, threadDelay)
import qualified FirstMaze
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Animate (animateIO)
import           Graphics.Gloss.Interface.IO.Display (controllerSetRedraw)
import qualified RecursiveMaze
import           Relude
import qualified Ui

main :: IO ()
main = do
  tmp <- newIORef Nothing
  a <- newEmptyMVar
  let grid = createGrid 15 15
  void $ forkIO $ void $ RecursiveMaze.doMaze 15 15 0 0 grid (\x -> putMVar a x >> threadDelay 50000)
--  void $ forkIO $ void $ FirstMaze.doMaze 15 15 0 0 grid (\x -> putMVar a x >> threadDelay 50000)
  animateIO
    (InWindow "Nice Window" (1900, 1200) (100, 100))
    white
    (\_ -> do
       b <- (fmap . fmap) Ui.drawGrid (tryTakeMVar a)
       case b of
         Just g  -> writeIORef tmp (Just g) >> return g
         Nothing -> fromMaybe blank <$> readIORef tmp)
    controllerSetRedraw
