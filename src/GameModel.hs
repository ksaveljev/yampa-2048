module GameModel ( emptyBoard
                 , initialGameState
                 , rotateBoard
                 , setTile
                 , tileToInt
                 , intToTile
                 , tilesWithCoordinates
                 , readTile
                 ) where

import System.Random (StdGen)
import Data.List (transpose)
import Control.Applicative ((<$>))

import Types

readTile :: (Row, Column) -> Board -> Tile
readTile (row, column) (Board b) = (b !! row) !! column

setTile :: (Row, Column) -> Board -> Tile -> Board
setTile (row, column) (Board b) tile =
    let r = b !! row
        nr = take column r ++ [tile] ++ drop (column + 1) r
    in Board $ take row b ++ [nr] ++ drop (row + 1) b

tileToInt :: Tile -> Int
tileToInt tile = case tile of
                   Number v -> v
                   Empty -> 0

intToTile :: Int -> Tile
intToTile n = case n of
                0 -> Empty
                _ -> Number n

tilesWithCoordinates :: Board -> [(Tile, Row, Column)]
tilesWithCoordinates (Board b) = concat
                               $ zipWith (\rowIndex row -> fmap (\(tile, columnIndex) -> (tile, rowIndex, columnIndex)) row) [0..]
                               $ fmap (\row -> zip row [0..])
                                 b

rotateBoard :: Board -> Board
rotateBoard (Board b) = Board $ reverse <$> transpose b

emptyBoard :: Board
emptyBoard = Board $ replicate 4 $ replicate 4 Empty

initialGameState :: StdGen -> GameState
initialGameState g = GameState { board = emptyBoard
                               , score = 0
                               , status = InProgress
                               , gen = g
                               }
