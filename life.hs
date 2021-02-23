import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Animate

import Life_backend

-- everything below is gloss stuff

main :: IO ()
main = do
    animate FullScreen white makePicture 

makePicture :: Float -> Picture
makePicture n = rectangleSolid n n

-- render new board after every second, gets passed the time in seconds since program start
-- renderBoard :: Float -> Picture



