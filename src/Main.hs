{-# LANGUAGE Arrows #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Yampa
import FRP.Yampa (returnA, Event, SF)

mainSF :: SF (Event InputEvent) Picture
mainSF = proc e -> do
  returnA -< blank

main :: IO ()
main = playYampa
         (InWindow "2048 game" (320, 240) (800, 200))
         white
         30
         mainSF
