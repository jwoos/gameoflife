module Main (main) where

-- Rules
-- Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overpopulation.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

grid x y = [[0 | y' <- [1..y]] | x' <- [1..x]]

main = print $ grid 100 100
