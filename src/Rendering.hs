module Rendering (drawBoard) where

import Graphics.Gloss

import GameModel
import Types

rowHeight :: Float
rowHeight = 100

tilePrecision :: Int
tilePrecision = 10

tileS :: Float
tileS = 90

tileRoundness :: Float
tileRoundness = 4

textScale :: Float
textScale = 0.2

tileBackColor :: Color
tileBackColor = makeColorI 205 192 180 255

roundedRect :: Int -> Float -> Float -> Float -> Picture
roundedRect n w h r = pictures [ drawQuarterRoundedRect n w h r
                               , rotate 90 $ drawQuarterRoundedRect n w h r
                               , rotate 180 $ drawQuarterRoundedRect n w h r
                               , rotate 270 $ drawQuarterRoundedRect n w h r
                               ]

getPoint :: Float -> Float -> Float -> Float -> (Float,Float)
getPoint x y r th = (x+r*cos th, y+r*sin th)

arcPath :: Int -> (Float,Float) -> Float -> Path
arcPath n (x,y) r = map (getPoint x y r) $ 0.0 : map (\v -> pi / 2 / fromIntegral v) (reverse [1..n+1])

quarterRoundedRect :: Int -> Float -> Float -> Float -> Path
quarterRoundedRect n w h r = [(0,0), (0,h/2)]
                          ++ reverse (arcPath n (w / 2 - r, h / 2 - r) r)
                          ++ [(w/2,0)]

drawQuarterRoundedRect :: Int -> Float -> Float -> Float -> Picture
drawQuarterRoundedRect n w h r = polygon $ quarterRoundedRect n w h r

drawTileBack :: Float -> Picture
drawTileBack x = color tileBackColor (translate x 0 (roundedRect tilePrecision tileS tileS tileRoundness))

-- Takes x-offset and tile and draws the tile itself
drawTile :: Float -> Tile -> Picture
drawTile x tile = 
    let background = [color (tileColor tile) $ roundedRect tilePrecision tileS tileS tileRoundness]
        number = if tileToInt tile > 0
                   then [translate (-20) (-10) $ scale textScale textScale $ text $ show $ tileToInt tile]
                   else []
        curScale = 1
    in pictures [ drawTileBack x
                , translate x 0 $ scale curScale curScale $ pictures $ background ++ number
                ]

drawRow :: [Tile] -> Picture
drawRow tile =
    let [i, j, k, l] = tile
    in translate (-300) 0 (pictures [ drawTile 0 i
                                    , drawTile rowHeight j
                                    , drawTile (rowHeight * 2) k
                                    , drawTile (rowHeight * 3) l
                                    ])

gameOverMessage :: Picture
gameOverMessage = pictures [ translate (-500) (-500) $ color translucentWhite $ rectangleSolid 2000 2000
                           , translate (-335) (-150) $ scale 0.5 0.5 $ color black $ text "Game Over"
                           ]
  where translucentWhite = makeColorI 255 255 255 150

drawBoard :: GameState -> Picture
drawBoard gameState =
    let (Board b) = board gameState
        [r1, r2, r3, r4] = b
    in translate 150 150
     $ pictures
     $ [ drawRow r1
       , translate 0 (-rowHeight) (drawRow r2)
       , translate 0 (-rowHeight * 2) (drawRow r3)
       , translate 0 (-rowHeight * 3) (drawRow r4)
       , translate (-300) 60 $ scale 0.2 0.2 $ color white $ text $ "Score: " ++ show (score gameState)
       ] ++ gameOverPicture
  where gameOverPicture = [gameOverMessage | status gameState == GameOver]


tileColor :: Tile -> Color
tileColor tile = case tile of
                   Number 2     -> makeColorI 238 228 218 255
                   Number 4     -> makeColorI 238 228 218 255
                   Number 8     -> makeColorI 238 228 218 255
                   Number 16    -> makeColorI 238 228 218 255
                   Number 32    -> makeColorI 238 228 218 255
                   Number 64    -> makeColorI 238 228 218 255
                   Number 128   -> makeColorI 238 228 218 255
                   Number 256   -> makeColorI 238 228 218 255
                   Number 512   -> makeColorI 238 228 218 255
                   Number 1024  -> makeColorI 238 228 218 255
                   Number 2048  -> makeColorI 238 228 218 255
                   Number 4096  -> makeColorI 238 228 218 255
                   Number 8192  -> makeColorI 238 228 218 255
                   Number 16384 -> makeColorI 238 228 218 255
                   Number 32768 -> makeColorI 238 228 218 255
                   Number 65536 -> makeColorI 238 228 218 255
                   _            -> makeColorI 238 228 218 90
