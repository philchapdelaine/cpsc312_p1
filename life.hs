import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Animate

import Life_backend

-- everything below is gloss stuff

window :: Display
window = InWindow "Game of Pobable Life" (800, 800) (1, 1)

background :: Color
background = white

makePicture :: Float -> Picture
makePicture n = rectangleSolid n n

main :: IO ()
main = do
    animate window background makePicture


-- render new board after every second, gets passed the time in seconds since program start
-- renderBoard :: Float -> Picture
