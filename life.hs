import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Animate
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game

import Life_backend

-- everything below is gloss stuff

-- window width and height in pixels
width = 800
height = 800

-- defines the window of the canvas: height, width and top left location
window :: Display
window = InWindow "Game of Probable Life" (width, height) (0, 0)

background :: Color
background = white

makePicture :: Float -> Picture
makePicture n = rectangleSolid n n

-- Description of a state of the world
data World = Game
    { aliveCells :: [Cell],
    prob :: Int,
    time :: Float,
    fps :: Int }

cell1 = Cell Alive (24,24)
cell2 = Cell Alive (24,25)
cell3 = Cell Alive (24,26)

cell4 = Cell Alive (24,25)
cell5 = Cell Alive (25,25)
cell6 = Cell Alive (26,25)
cell7 = Cell Alive (24,26)
cell8 = Cell Alive (25,27)

cell9 = Cell Alive (25,24)
cell10 = Cell Alive (26,24)
cell11 = Cell Alive (24,25)
cell12 = Cell Alive (25,25)
cell13 = Cell Alive (25,26)

cell14 = Cell Alive (24,25)
cell15 = Cell Alive (25,25)
cell16 = Cell Alive (26,25)
cell17 = Cell Alive (25,24)

cell18 = Cell Alive (23,24)
cell19 = Cell Alive (22,26)
cell20 = Cell Alive (23,26)
cell21 = Cell Alive (25,25)
cell22 = Cell Alive (26,26)
cell23 = Cell Alive (27,26)
cell24 = Cell Alive (28,26)

state1 = [cell1, cell2, cell3] -- spinner
state2 = [cell4, cell5, cell6, cell7, cell8] -- glider
state3 = [cell9, cell10, cell11, cell12, cell13] -- f-pentomino
state4 = [cell14, cell15, cell16, cell17] -- tetromino
state5 = [cell18, cell19, cell20, cell21, cell22, cell23, cell24] -- acorn
-- Initial state of the board
initialWorld = Game
    {
    aliveCells = state1, -- TODO add user input
    prob = 100,
    time = 0.0,
    fps = 1 }

main :: IO ()
main = playIO
  window
  background
  1
  initialWorld
  drawingFunc
  inputHandler
  updateFunc

drawingFunc :: World -> IO Picture
drawingFunc world = return (Pictures $ grid ++ aliveCellsPictures ++ [printProb (prob world)])
   where aliveCellsPictures = [drawCell (position) | (Cell state position) <- aliveCells  world]


-- Defines a grid space
w = (fromIntegral width)
h = (fromIntegral height)

-- x, y cells
x = 50
y = 50

-- defines the grid and the size of each cell
grid = verticalLines ++ horizontalLines ++ [rectangleWire w h]
    where verticalLines = foldr (\a -> \b -> vLine a:b) [] [0..x]
          vLine a = color  (greyN 0.5)  (line [ (w/x*a-w/2, -h/2), (w/x*a-w/2, h-h/2) ])
          horizontalLines = foldr (\a -> \b -> hLine a:b) [] [0..y]
          hLine a = color  (greyN 0.5)  (line [ (-w/2, h/y*a-h/2), (w-w/2, h/y*a-h/2) ])

drawCell (x0,y0) =  translate (x0*w/x -w/2 +  w/x/2) (-y0*h/y +h/2 -h/y/2) square

-- Filled in cells look like this
square = rectangleSolid (w/x) (h/y)

inputHandler :: Event -> World -> IO World
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) world = return world {prob = if (prob world > 0) then (-10 + prob world) else (prob world)}
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) world = return world {prob = if (prob world < 100) then (10 + prob world) else (prob world)}
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) world = return world {prob = 100}
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) world = return world {prob = 0}
inputHandler (EventKey (Char '1') Down _ _) world = return world {aliveCells = state1}
inputHandler (EventKey (Char '2') Down _ _) world = return world {aliveCells = state2}
inputHandler (EventKey (Char '3') Down _ _) world = return world {aliveCells = state3}
inputHandler (EventKey (Char '4') Down _ _) world = return world {aliveCells = state4}
inputHandler (EventKey (Char '5') Down _ _) world = return world {aliveCells = state5}
inputHandler _ w = return w



updateFunc :: Float -> World -> IO World
updateFunc _ world =
      do
        newResult <- Life_backend.gameOfLife (aliveCells world) (prob world)
        return world { aliveCells = newResult }

-- prints the probability on the grid
printProb prob
  = Translate (-70) (-280)
  $ Scale 0.25 0.25
  $ Text ("probability is: " ++ show prob)

-- render new board after every second, gets passed the time in seconds since program start
-- renderBoard :: Float -> Picture
