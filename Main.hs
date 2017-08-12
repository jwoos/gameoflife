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

setStatus :: Array (Integer, Integer) Status -> [(Integer, Integer)] -> Status -> Array (Integer, Integer) Status
setStatus arr indices stat = arr // ([(x, stat) | x <- indices])

setAlive :: Array (Integer, Integer) Status -> [(Integer, Integer)] -> Array (Integer, Integer) Status
setAlive arr indices = setStatus arr indices Alive

setDead :: Array (Integer, Integer) Status -> [(Integer, Integer)] -> Array (Integer, Integer) Status
setDead arr indices = setStatus arr indices Dead

setUp :: (Integer, Integer) -> Board
setUp (x, y) = array bounds $ zip (range bounds) (repeat Dead)
  where bounds = ((0, 0), (x - 1, y - 1))

main :: IO ()
main = putStrLn "hello"
