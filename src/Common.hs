{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Common where

import qualified Data.Vector           as V
import           Lens.Micro.Platform
import           Relude                hiding (Down, Left, Right)
import           System.Random         (Random (..))
import           System.Random.Shuffle

data Cell =
  Cell
    { _cellId         :: Int
    , _topConnection  :: Bool
    , _leftConnection :: Bool
    , _visited        :: Bool
    , _current        :: Bool
    }
  deriving (Show)

makeLenses ''Cell

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
  Grid "" (V.fromList [V.fromList ((\x -> Cell (x * i) False False False False) <$> [1 .. x]) | i <- [1 .. y]]) x y

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
