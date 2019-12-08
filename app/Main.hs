{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Ascii
import           Common
import           Control.Concurrent                  (forkIO, threadDelay)
import qualified FirstMaze
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Animate (animateIO)
import           Graphics.Gloss.Interface.IO.Display (controllerSetRedraw)
import qualified RecursiveMaze
import           Relude
import           System.Random                       as Random
import qualified Ui

--main :: IO ()
--main = do
--  tmp <- newIORef Nothing
--  a <- newEmptyMVar
--  let grid = createGrid 25 25
--  (x, y) <- (,) <$> Random.randomRIO (0, 14) <*> Random.randomRIO (0, 14)
--  void $ forkIO $ void $ RecursiveMaze.doMaze x y grid (putMVar a)
--  --void $ forkIO $ void $ FirstMaze.doMaze 0 0 grid (\x -> putMVar a x >> threadDelay 50000)
--  animateIO
--    (InWindow "Nice Window" (1900, 1200) (100, 100))
--    white
--    (\_ -> do
--       b <- (fmap . fmap) Ui.drawGrid (tryTakeMVar a)
--       case b of
--         Just g  -> writeIORef tmp (Just g) >> return g
--         Nothing -> fromMaybe blank <$> readIORef tmp)
--    controllerSetRedraw
--main = showMaze 25 25 (\x y g -> RecursiveMaze.doMaze x y g (\_ -> pure ()))
--line1 x y = polygon [(-x / 2, -y / 2), (x / 2, -y / 2), (x / 2, y / 2), (-x / 2, y / 2)]
--

main :: IO ()
main = do
  let g = createGrid 50 50
  a <- RecursiveMaze.doMaze 0 0 g (\_ -> pure ())
  display (InWindow "adsad" (800, 800) (200, 200)) white (Ui.drawGrid a)
