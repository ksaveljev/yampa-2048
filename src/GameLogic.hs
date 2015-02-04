module GameLogic ( placeRandomTile
                 , slideBoard
                 , isGameOver
                 ) where

import Prelude ((==), (/=), (>), (*), (<), fromIntegral, floor, fst, snd, ($), Float, Int, Bool, Eq, error)
import Data.List (and, length, sum, filter, concat, (++), replicate, take, (!!))
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Functor (fmap)
import Control.Applicative ((<$>))
import Control.Category ((.), id)

import Types
import GameModel

tile2Probability :: Float
tile2Probability = 0.9

newTile :: Float -> Tile
newTile x = Number $ if x < tile2Probability then 2 else 4

emptyTiles :: Board -> [(Row, Column)]
emptyTiles b = fmap (\(_, r, c) -> (r, c))
             $ filter (\(tile, _, _) -> tile == Empty)
             $ tilesWithCoordinates b

newTileIndex :: Float -> Board -> Maybe (Row, Column)
newTileIndex x b =
    let emptyTileIndices = emptyTiles b
    in case emptyTileIndices of
         [] -> Nothing
         _ -> Just $ emptyTileIndices !! floor (fromIntegral (length emptyTileIndices) * x)

placeRandomTile :: Float -> Float -> GameState -> GameState
placeRandomTile float1 float2 gameState =
    let tileIndex = newTileIndex float1 (board gameState)
    in if isNothing tileIndex
         then gameState
         else gameState { board = setTile (fromMaybe (0, 0) tileIndex) (board gameState) $ newTile float2 }

groupedByTwo :: Eq a => [a] -> [[a]]
groupedByTwo l = case l of
                   [x] -> [[x]]
                   [x, y] -> if x == y then [[x, y]] else [[x], [y]]
                   (x:y:xs) -> if x == y
                                 then [x, y] : groupedByTwo xs
                                 else [x] : groupedByTwo (y : xs)
                   _ -> []

slideRow :: [Tile] -> ([Tile], Int)
slideRow row = let grouped = groupedByTwo $ filter (/= Empty) row
               in ( take 4
                    $ fmap (intToTile . sum . fmap tileToInt) grouped ++ replicate 4 Empty
                  , sum . fmap tileToInt $ concat $ filter (\x -> length x > 1) grouped
                  )

slideBoard :: Direction -> Board -> (Board, Int)
slideBoard direction b =
    let rotatedBoard = (case direction of
                          Down -> rotateBoard
                          Right -> rotateBoard . rotateBoard
                          Up -> rotateBoard . rotateBoard . rotateBoard
                          Left -> id
                          _ -> error "unexpected direction") b

        rowsWithScores = slideRow <$> (\(Board x) -> x) rotatedBoard

        slidRotatedBoard = Board $ fmap fst rowsWithScores
        scoreGained = sum $ fmap snd rowsWithScores

        slidBoard = (case direction of
                       Up -> rotateBoard
                       Right -> rotateBoard . rotateBoard
                       Down -> rotateBoard . rotateBoard . rotateBoard
                       Left -> id
                       _ -> error "unexpected direction") slidRotatedBoard

    in (slidBoard, scoreGained)

isGameOver :: GameState -> Bool
isGameOver gameState =
    let b = board gameState
        slidUp = fst $ slideBoard Up b
        slidDown = fst $ slideBoard Down b
        slidLeft = fst $ slideBoard Left b
        slidRight = fst $ slideBoard Right b
    in and [ b /= emptyBoard
           , slidUp == slidDown
           , slidDown == slidLeft
           , slidLeft == slidRight
           , slidRight == b
           ]
