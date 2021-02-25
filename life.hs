import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Animate

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

main :: IO ()
main = do
    animate
    window -- window
    background -- background color
    makePicture -- starting state of the board


-- render new board after every second, gets passed the time in seconds since program start
-- renderBoard :: Float -> Picture
