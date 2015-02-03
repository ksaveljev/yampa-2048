module Types ( InputEvent
             , Tile(..)
             , Row
             , Board
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

data Tile = Tile { value :: Int
                 , popInTime :: Int
                 , popOutTime :: Int
                 } deriving (Eq, Show)

type Row = [Tile]

type Board = [Row]

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
