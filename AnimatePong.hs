module AnimatePong where

import FRP.Yampa
import FRP.Yampa.Geometry
import FRP.Yampa.Event

import Pong
import Animator
import AnimateBalls hiding (main)

import Graphics.Blank hiding (Event)

-- set browser to: http://localhost:3001/

import Balls

main :: IO ()
main = blankCanvas 3000 $ \ canvas -> do {q <- events canvas MouseDown ;runSFcanvas (clickNewBall canvas q) pongExample renderBallsAndPaddle canvas}
--runSFcanvas (clickNewBall canvas q) pongExample renderBallsAndPaddle canvas
								 

renderBallsAndPaddle :: ([Ball2],Paddle,Spaceship,Spaceship) -> Canvas ()
renderBallsAndPaddle (bs,p,s,s2) = renderBalls bs >> renderPaddle p >> renderSpaceship s >> renderSpaceship s2

renderPaddle :: Paddle -> Canvas ()
renderPaddle p = do x <- scaleX (padLeftEdge p)
                    y <- scaleY (padTopEdge p)
                    w  <- scaleWidth (padWidth p)
                    h  <- scaleHeight (padHeight p)
                    rectangle x y w h (padCol p)
				
{-rendership :: Canvas ()
rendership = do x <- scaleX 0.5
                y <- scaleY 0.8
                w  <- scaleWidth 0.06
                h  <- scaleHeight 0.03
                rectangle x y w h "red" -}
			 
renderSpaceship :: Spaceship -> Canvas()
renderSpaceship s = do {x <- scaleX (shipLeftEdge s);
                       y <- scaleY (shipTopEdge s);
                       w  <- scaleWidth (shipWidth s);
                       h  <- scaleHeight (shipHeight s);
                       rectangle x y w h (shipCol s)}
