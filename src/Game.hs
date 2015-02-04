{-# LANGUAGE Arrows #-}

module Game (wholeGame) where

import System.Random (StdGen)
import FRP.Yampa

import Types
import GameModel
import GameLogic

wholeGame :: StdGen -> SF GameInput GameState
wholeGame g = switch
  (gameAlive g >>> (identity &&& outOfMoves))
  (restartGame g)

outOfMoves :: SF GameState (Event GameState)
outOfMoves = proc s -> do
  lost <- edge -< isGameOver s
  let snapshot = lost `tag` s
  returnA -< snapshot

gameAlive :: StdGen -> SF GameInput GameState
gameAlive g = 
    let (float1, g') = random g
        (float2, g'') = random g'
        (float3, g''') = random g''
        (float4, g'''') = random g'''
    in runGame $ placeRandomTile float1 float2
               $ placeRandomTile float3 float4
                 (initialGameState g'''')

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

runGame :: GameState -> SF GameInput GameState
runGame state = proc input -> do
  rec currentState <- dHold state -< gameUpdated
      gameUpdated <- arr update -< (currentState, input)

  returnA -< currentState

update :: (GameState, GameInput) -> Event GameState
update (gameState, input) =
    case input of
      Event None -> NoEvent
      Event direction ->
        let newBoardScore = slideBoard direction (board gameState)
            (float1, gen') = random (gen gameState)
            (float2, gen'') = random gen'
        in if fst newBoardScore == board gameState
             then NoEvent
             else Event $ placeRandomTile float1 float2
                        $ gameState { board = fst newBoardScore
                                    , score = score gameState + snd newBoardScore
                                    , gen = gen''
                                    }
      _ -> NoEvent
