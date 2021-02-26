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
window = InWindow "Game of Pobable Life" (width, height) (0, 0)

background :: Color
background = white

makePicture :: Float -> Picture
makePicture n = rectangleSolid n n

-- Description of a state of the world
data World = Game
    { aliveCells :: [(Float,Float)],
    prob :: Int,
    time :: Float,
    fps :: Int	}

-- Initial state of the board
initialWorld = Game
    {
    aliveCells = [(1,1),(2,2)], -- TODO add user input
    prob = 1,
    time = 0.0,
    fps = 20	}

main :: IO ()
main = play
  window
  background
  20
  initialWorld
  drawingFunc
  inputHandler
  updateFunc

drawingFunc :: World -> Picture
drawingFunc world = Pictures $ grid ++ aliveCellsPictures ++ [printProb (prob world)]
   where aliveCellsPictures = [drawCell (a,b) | (a,b) <- aliveCells world]

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

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) world = world {prob = 0}
inputHandler (EventKey (SpecialKey KeyUp) Up _ _) world = world {prob = 1}
inputHandler _ w = w

updateFunc :: Float -> World -> World
updateFunc _ world = world -- { aliveCells = (nextBoardGen (world { aliveCells }) world { prob } [1,0,1])}

-- prints the probability on the grid
printProb prob
	= Translate (-70) (-280)
	$ Scale 0.25 0.25
	$ Text ("probability is: " ++ show prob)

-- render new board after every second, gets passed the time in seconds since program start
-- renderBoard :: Float -> Picture
