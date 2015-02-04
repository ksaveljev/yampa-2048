{-# LANGUAGE Arrows #-}

module Game (wholeGame) where

import Prelude (Eq, ($), const, Int, Bool, Float, (==), (+), (<), (/=), (>), (*), fst, snd, floor, fromIntegral)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.List (and, replicate, reverse, transpose, (!!), (++), drop, take, zipWith, length, sum, concat, zip, filter)
import Control.Applicative ((<$>))
import Control.Category ((.), id)
import Data.Functor (fmap)
import System.Random (StdGen)
import FRP.Yampa

import Types

tile2Probability :: Float
tile2Probability = 0.9

emptyBoard :: Board
emptyBoard = Board $ replicate 4 $ replicate 4 Empty

readTile :: (Row, Column) -> Board -> Tile
readTile (row, column) (Board b) = (b !! row) !! column

setTile :: (Row, Column) -> Board -> Tile -> Board
setTile (row, column) (Board b) tile =
    let r = b !! row
        nr = take column r ++ [tile] ++ drop (column + 1) r
    in Board $ take row b ++ [nr] ++ drop (row + 1) b

newTile :: Float -> Tile
newTile x = Number $ if x < tile2Probability then 2 else 4

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
                               $ zipWith (\columnIndex row -> fmap (\(tile, rowIndex) -> (tile, rowIndex, columnIndex)) row) [0..]
                               $ fmap (\row -> zip row [0..])
                                 b

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

rotateBoard :: Board -> Board
rotateBoard (Board b) = Board $ reverse <$> transpose b

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
                          _ -> id) b

        rowsWithScores = slideRow <$> (\(Board x) -> x) rotatedBoard

        slidRotatedBoard = Board $ fmap fst rowsWithScores
        scoreGained = sum $ fmap snd rowsWithScores

        slidBoard = (case direction of
                       Up -> rotateBoard
                       Right -> rotateBoard . rotateBoard
                       Down -> rotateBoard . rotateBoard . rotateBoard
                       _ -> id) slidRotatedBoard

    in (slidBoard, scoreGained)

placeRandomTile :: Float -> Float -> GameState -> GameState
placeRandomTile float1 float2 gameState =
    let tileIndex = newTileIndex float1 (board gameState)
    in if isNothing tileIndex
         then gameState
         else gameState { board = setTile (fromMaybe (0, 0) tileIndex) (board gameState) $ newTile float2 }

initialGameState :: StdGen -> GameState
initialGameState g = GameState { board = emptyBoard
                               , score = 0
                               , status = InProgress
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
gameAlive g = runGame (initialGameState g)

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
  rec currentState <- hold state -< gameUpdated
      gameUpdated <- arr update -< (currentState, input)

  returnA -< currentState

update :: (GameState, GameInput) -> Event GameState
update (gameState, input) =
    case input of
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
