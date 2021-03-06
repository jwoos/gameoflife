module Main (
  main,
  setUp
            ) where

import Control.Monad
import Data.Array
import Data.List
import qualified Data.Text as Text
import System.Environment

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty
import System.Console.Terminal.Size


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
setUp (x, y) = array bounds [(c, Dead) | c <- range bounds]
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

handlEvent :: Board -> BrickEvent n GenerationEvent -> EventM n (Next Board)
handlEvent board (VtyEvent (EvKey (KChar 'q') [])) = halt board
handlEvent board (VtyEvent (EvResize _ _)) = halt board
handlEvent board _ = continue (next board)

draw :: Board -> [Widget ()]
draw board = [withBorderStyle unicode $ border $ center (str $ intercalate "\n" allText)]
{-draw board = [centerWith (Just '|') $ (str $ intercalate "\n" allText)]-}
  where (_, (maxX, maxY)) = bounds board
        all = [[if val == Dead then " " else "■" | y <- [0..maxY], let val = board ! (x, y)] | x <- [0..maxX]]
        allText = [intercalate "" t | t <- all]

generationThread :: BChan GenerationEvent -> IO ()
generationThread chan = forever $ do
    writeBChan chan Generation

main :: IO Board
main = do
  winSizeIO <- size
  args <- getArgs
  let (Just winSize) = winSizeIO
      rows = height winSize
      cols = width winSize
      config = if null args then [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)] else [(read $ Text.unpack x, read $ Text.unpack y) | coord <- args, let lst = Text.splitOn (Text.pack ",") (Text.pack coord), let x = head lst, let y = last lst]
      startState = initial (setUp (rows, cols)) config
   in customMain (mkVty defaultConfig) Nothing app startState
  where app = App {
        appDraw = draw,
        appHandleEvent = handlEvent,
        appStartEvent = return,
        appAttrMap = const $ attrMap defAttr [],
        appChooseCursor = neverShowCursor
                  }
