import System.Random (newStdGen, StdGen)
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Interface.FRP.Yampa
import FRP.Yampa (Event(..), SF, arr, tag, (>>>))

import Types
import GameModel
import Game

drawGame :: SF GameState Picture
drawGame = undefined

parseInput :: SF (Event InputEvent) GameInput
parseInput = arr $ \event ->
  case event of
    Event (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> event `tag` Types.Up
    Event (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) -> event `tag` Types.Down
    Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> event `tag` Types.Left
    Event (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) -> event `tag` Types.Right
    _ -> event `tag` None

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
