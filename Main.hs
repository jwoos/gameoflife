module Main (
  main,
  setUp
            ) where

import Data.Array
import Data.List
import qualified Data.Text as Text

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty


-- Rules
-- Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overpopulation.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

data Status = Alive | Dead deriving (Eq, Show)
type Board = Array (Integer, Integer) Status

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

underpopulate :: Board -> [(Integer, Integer)] -> [(Integer, Integer)]
underpopulate board alive = [coord | coord <- alive, (length $ getAliveNeighbors board coord) <= 1]

overpopulate :: Board -> [(Integer, Integer)] -> [(Integer, Integer)]
overpopulate board alive = [coord | coord <- alive, (length $ getAliveNeighbors board coord) >= 4]

populate :: Board -> [(Integer, Integer)] -> [(Integer, Integer)]
populate board dead = [coord | coord <- dead, (length $ getAliveNeighbors board coord) == 3]

setUp :: (Integer, Integer) -> Board
setUp (x, y) = array bounds [(c, Dead) | c <- range(bounds)]
  where bounds = ((0, 0), (x - 1, y - 1))

initial :: Board -> [(Integer, Integer)] -> Board
initial board coords = board // toApply
  where toApply = [(x, Alive) | x <- coords]

next :: Board -> Board
next board = board // changes
  where allCoords = range (bounds board)
        alive = getAlive board
        dead = getDead board
        willDie = zip (underpopulate board alive ++ overpopulate board alive) (repeat Dead)
        willGrow = zip (populate board dead) (repeat Alive)
        changes = willDie ++ willGrow

data GenerationEvent = Generation

{-toString :: Board -> [Widget n]-}
{-toString board = Text.intercalate-}
  {-where list = [x | x <- ]-}
    {-text = Text.pack board-}

handlEvent :: Board -> BrickEvent n GenerationEvent -> EventM n (Next Board)
handlEvent board (VtyEvent (EvKey (KChar 'q') [])) = halt board
handlEvent board _ = continue (next board)

draw :: Board -> [Widget ()]
draw board = [withBorderStyle unicode $ border $ center (str $ intercalate "\n" allText)]
  where (_, (maxX, maxY)) = bounds board
        all = [[if val == Dead then "0" else "1" | y <- [1..maxY], let val = board ! (x, y)] | x <- [1..maxX]]
        allText = [intercalate "" t | t <- all]

quitNoResize :: Board -> BrickEvent n e -> EventM n (Next Board)
quitNoResize board (VtyEvent (EvResize _ _)) = continue board
quitNoResize board _ = halt board

ui :: Widget ()
ui = withBorderStyle unicode $
  border $
    center (str "Hello world")

main :: IO Board
main = do
  customMain (mkVty defaultConfig) Nothing app initial
    where app = App {
        appDraw = const [ui],
        appHandleEvent = handlEvent,
        appStartEvent = return,
        appAttrMap = const $ attrMap defAttr [],
        appChooseCursor = neverShowCursor
                    }
          initial = setUp (25, 25)
