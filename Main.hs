module Main (
  main,
  setUp
  ) where

import Data.Array


-- Rules
-- Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overpopulation.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

data Status = Alive | Dead deriving (Eq, Show)
type Board = Array (Integer, Integer) Status

set :: Board -> [((Integer, Integer), Status)] -> Board
set board = (//) board

get :: Board -> [((Integer, Integer), Status)] -> Board
get board = []

setStatus :: Board -> [(Integer, Integer)] -> Status -> Board
setStatus board indices stat = board // ([(x, stat) | x <- indices])

setAlive :: Board -> [(Integer, Integer)] -> Board
setAlive board indices = setStatus board indices Alive

setDead :: Board -> [(Integer, Integer)] -> Board
setDead board indices = setStatus board indices Dead

getByState :: Board -> Status -> [(Integer, Integer)]
getByState board status = [coord | coord <- allCoords, (board ! coord) == status]
  where allCoords = range (bounds board)

getAlive :: Board -> [(Integer, Integer)]
getAlive board = getByState board Alive

getDead :: Board -> [(Integer, Integer)]
getDead board = getByState board Dead

getNeighbors :: Board -> (Integer, Integer) -> [(Integer, Integer)]
getNeighbors board coord@(x, y) = [coord' | x' <- [x - 1..x + 1], y' <- [y - 1..y + 1], let coord' = (x', y'), coord' /= coord, inRange (bounds board) coord']

getNeighborsByStatus :: Board -> (Integer, Integer) -> Status -> [(Integer, Integer)]
getNeighborsByStatus board coord status = [index | index <- allNeighbors, (board ! index) == status]
  where allNeighbors = getNeighbors board coord

getAliveNeighbors :: Board -> (Integer, Integer) -> [(Integer, Integer)]
getAliveNeighbors board coord = getNeighborsByStatus board coord Alive

getDeadNeighbors :: Board -> (Integer, Integer) -> [(Integer, Integer)]
getDeadNeighbors board coord = getNeighborsByStatus board coord Dead

setUp :: (Integer, Integer) -> Board
setUp (x, y) = array bounds [(x, Dead) | x <- range(bounds)]
  where bounds = ((0, 0), (x - 1, y - 1))

underpopulate :: Board -> [(Integer, Integer)] -> [(Integer, Integer)]
underpopulate board alive = [coord | coord <- alive, (length $ getAliveNeighbors board coord) <= 1]

overpopulate :: Board -> [(Integer, Integer)] -> [(Integer, Integer)]
overpopulate board alive = [coord | coord <- alive, (length $ getAliveNeighbors board coord) >= 4]

populate :: Board -> [(Integer, Integer)] -> [(Integer, Integer)]
populate board dead = [coord | coord <- dead, (length $ getAliveNeighbors board coord) == 2]

next :: Board -> Board
next board = board
  where allCoords = range (bounds board)
        alive = getAlive board
        dead = getDead board
        willDie = underpopulate board alive : overpopulate board alive
        willGrow = populate board dead

main :: IO ()
main = putStrLn "hi"
