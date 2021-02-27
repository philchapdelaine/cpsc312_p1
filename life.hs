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

cell1 = Cell Alive (3,2)
cell2 = Cell Alive (3,3)
cell3 = Cell Alive (3,4)
cell4 = Cell Alive (4,3)
cell5 = Cell Alive (2,4)
cell6 = Cell Alive (1,2)

-- Initial state of the board
initialWorld = Game
    {
    aliveCells = [cell1, cell2, cell3, cell5, cell6], -- TODO add user input
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
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) world = return world {prob = 0 + prob world}
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) world = return world {prob = 10 + prob world}
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
