{-# LANGUAGE Arrows #-}

module Game (wholeGame) where

import Prelude (($), const, Int, Bool, (&&), (==), (+))
import Data.List (replicate, transpose, (!!), (++), drop, take)
import Control.Category ((.), id)
import Data.Functor (fmap)
import System.Random (StdGen)
import FRP.Yampa

import Types

emptyBoard :: Board
emptyBoard = Board $ fmap (fmap makeTile) $ replicate 4 . replicate 4 Empty

makeTile :: TileValue -> Tile
makeTile v = Tile {value = v, popInTime = 0.0, popOutTime = 0.0}

initialGameState :: StdGen -> GameState
initialGameState g = addTile
                   . addTile
                     GameState { board = emptyBoard
                               , score = 0
                               , status = GamePlaying
                               , gen = g
                               }

wholeGame :: StdGen -> SF GameInput GameState
wholeGame g = switch
  (gameAlive g >>> (arr id &&& outOfMoves))
  (restartGame g)

outOfMoves :: SF GameState (Event GameState)
outOfMoves = proc s -> do
  lost <- edge -< isGameOver s
  let snapshot = lost `tag` s
  returnA -< snapshot

gameAlive :: StdGen -> SF GameInput GameState
gameAlive g = runGame (initialGameState g) 0

-- | When the game is lost we want to show the GameOver text for some time
-- and then restart the game
restartGame :: StdGen -> GameState -> SF GameInput GameState
restartGame g s = switch
  (gameOver s &&& after 3 ())
  (const $ wholeGame g)

-- | When we have lost the game we want to keep the board in a state that
-- the user reached and show some GameOver message over it
gameOver :: GameState -> SF a GameState
gameOver s = arr $ const $ s { status = GameOver }

runGame :: GameState -> Int -> SF GameInput GameState
runGame state score = proc input -> do
  rec currentState <- hold state -< gameUpdated
      gameUpdated <- arr update -< (currentState, input)

  returnA -< currentState

update :: (GameState, GameInput) -> Event GameState
update (gameState, input) =
    -- TODO: implement this stuff
    case input of
      Up -> NoEvent
      Down -> NoEvent
      Left -> NoEvent
      Right -> NoEvent
      None -> NoEvent

isGameOver :: GameState -> Bool
isGameOver gameState = move Up gameState == gameState
                    && move Down gameState == gameState
                    && move Left gameState == gameState
                    && move Right gameState == gameState

readTile :: (Row, Column) -> Board -> Tile
readTile (row, column) (Board b) = (b !! row) !! column

setTile :: (Row, Column) -> Board -> Tile -> Board
setTile (row, column) (Board b) tile =
    let r = b !! row
        nr = (take column r) ++ [tile] ++ (drop (column + 1) r)
    in Board $ (take row b) ++ [nr] ++ (drop (row + 1) b)
