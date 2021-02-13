import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Animate

main :: IO ()
main = do
    animate FullScreen white makePicture

makePicture :: Float -> Picture
makePicture n = rectangleSolid n n


