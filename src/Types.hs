module Types ( InputEvent
             , Tile(..)
             , TileValue(..)
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

data TileValue = Empty | Value Int deriving Eq

data Tile = Tile { value :: TileValue
                 , popInTime :: Int
                 , popOutTime :: Int
                 } deriving Eq

type Row = Int
type Column = Int

newtype Board = Board [[Tile]]

data GameStatus = GamePlaying
                | GameOver
                deriving Eq

data GameState = GameState { board :: Board
                           , score :: Int
                           , status :: GameStatus
                           , gen :: StdGen
                           }

data Direction = Up | Down | Left | Right | None

type GameInput = Event Direction
