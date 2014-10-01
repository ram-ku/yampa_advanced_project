module AnimateBall where

import Ball
import Animator

import Graphics.Blank

-- set browser to: http://localhost:3000/

main :: IO ()
main = blankCanvas 3000 $ runSFcanvas (return ()) bouncingBallExample renderBall

renderBall :: Ball -> Canvas ()
renderBall b = do x <- scaleX (ballPosX b)
                  y <- scaleY (ballPosY b)
                  r <- scaleHeight (ballRad b)
                  circle x y r "black"
