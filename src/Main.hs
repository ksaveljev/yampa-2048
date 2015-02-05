import System.Random (newStdGen, StdGen)
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Interface.FRP.Yampa
import FRP.Yampa (Event(..), SF, arr, tag, (>>>))

import Types
import Game
import Rendering

-- | Our game uses up, down, left and right arrows to make the moves, so
-- the first thing we want to do is to parse the Gloss Event into something
-- we are happy to work with (Direction data type)
parseInput :: SF (Event InputEvent) GameInput
parseInput = arr $ \event ->
  case event of
    Event (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> event `tag` Types.Up
    Event (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) -> event `tag` Types.Down
    Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> event `tag` Types.Left
    Event (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) -> event `tag` Types.Right
    _ -> event `tag` None

-- | After parsing the game input and reacting to it we need to draw the
-- current game state which might have been updated
drawGame :: SF GameState Picture
drawGame = arr drawBoard

-- | Our main signal function which is responsible for handling the whole
-- game process, starting from parsing the input, moving to the game logic
-- based on that input and finally drawing the resulting game state to
-- Gloss' Picture
mainSF :: StdGen -> SF (Event InputEvent) Picture
mainSF g = parseInput >>> wholeGame g >>> drawGame

main :: IO ()
main = do
    g <- newStdGen
    playYampa
      (InWindow "2048 game" (410, 500) (200, 200))
      white
      30
      (mainSF g)
