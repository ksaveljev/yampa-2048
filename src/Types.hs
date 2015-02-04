module Types ( InputEvent
             , Tile(..)
             , Row
             , Column
             , Board(..)
             , GameState(..)
             , GameStatus(..)
             , Direction(..)
             , GameInput
             ) where

import System.Random (StdGen)
import FRP.Yampa (Event)
import qualified Graphics.Gloss.Interface.IO.Game as G

-- | A usefule type synonym for Gloss event values, to avoid confusion
-- between Gloss and Yampa.
type InputEvent = G.Event

data Tile = Number Int | Empty deriving (Eq, Show)

type Row = Int
type Column = Int

newtype Board = Board [[Tile]] deriving (Eq, Show)

data GameStatus = InProgress
                | GameOver
                deriving (Eq, Show)

data GameState = GameState { board :: Board
                           , score :: Int
                           , status :: GameStatus
                           , gen :: StdGen
                           } deriving (Show)

data Direction = Up | Down | Left | Right | None deriving (Eq, Show)

type GameInput = Event Direction
