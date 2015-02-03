{-# LANGUAGE Arrows #-}

module Game (wholeGame) where

import System.Random (StdGen)
import FRP.Yampa

import Types

emptyBoard :: Board
emptyBoard = fmap (fmap makeTile) $ replicate 4 . replicate 4 0

makeTile :: Int -> Tile
makeTile i = Tile {value = i, popInTime = 0.0, popOutTime = 0.0}

initialGameState :: StdGen -> GameState
initialGameState g = addTile
                   $ addTile
                   $ GameState { board = emptyBoard
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
  lost <- edge -< not $ canMove s
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
