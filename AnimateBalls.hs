module AnimateBalls where

import FRP.Yampa
import FRP.Yampa.Geometry
import FRP.Yampa.Event

import Balls
import Animator

import Graphics.Blank hiding (Event)

-- set browser to: http://localhost:3000/

main :: IO ()
main = blankCanvas 3000 $ \ canvas -> do {q <- events canvas MouseDown;
                                          {-print $ do {me1 <- tryReadEventQueue q;
										    case me1 of 
											nothing -> nothing
											Just (x,y) -> (x,y)};-}
								  runSFcanvas (clickNewBall canvas q) newBallsExample renderBalls canvas}
								 --runSFcanvas (return noEvent) newBallsExample renderBalls canvas

clickNewBall :: Context -> EventQueue -> IO (Event Position2)
clickNewBall canvas q = do {me <- tryReadEventQueue q;
					   send canvas $ do {bx <- toscaleX 0;by <- toscaleY 1;return (Event (vector2 bx by))};
					   case me >>= jsMouse of
						Nothing    -> return noEvent
						Just (x,y) -> send canvas $ do {bx <- toscaleX x;by <- toscaleY y;return (Event (vector2 bx by))}}
						
--findevent =do {e <- repeatedly 0.5 (vector2 0.5 0.5)}
{-findevent = e
			where 
				arr (Event e)= repeatedly 0.5 (vector2 0.5 0.5)
				-}

renderBall :: Ball2 -> Canvas ()
renderBall b = do x <- scaleX (ballPosX b)
                  y <- scaleY (ballPosY b)
                  r <- scaleLength (ballRad b)
                  circle x y r (ballCol b)

renderBalls :: [Ball2] -> Canvas ()
renderBalls = mapM_ renderBall
