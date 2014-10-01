module Animator where

import FRP.Yampa

import Data.Time.Clock
import Data.IORef

import Graphics.Blank hiding (scale)

type Height = Float
type Width = Float
type Radius = Float
type XCo = Float
type YCo = Float
type Colour = String
type Scale = Double

circle :: XCo -> YCo -> Radius -> Colour -> Canvas ()
circle x y r col = do beginPath ()
                      arc (x,y,r,0,pi*2,False)
                      closePath ()
                      fillStyle col
                      fill ()

-- If you've got the very latest version of blank-canvas, then this can replace the "rectangle" definition.
--
-- rectangle :: XCo -> YCo -> Width -> Height -> Colour -> Canvas ()
-- rectangle x y w h col = do fillStyle col
--                            fillRect (x,y,w,h)

rectangle :: XCo -> YCo -> Width -> Height -> Colour -> Canvas ()
rectangle x y w h col = do beginPath ()
                           moveTo (x,y)
                           lineTo (x+w,y)
                           lineTo (x+w,y+h)
                           lineTo (x,y+h)
                           lineTo (x,y)
                           closePath ()
                           fillStyle col
                           fill ()

scale :: Scale -> Float -> Float
scale s f = realToFrac s * f

scaleLength :: Scale -> Canvas Float
scaleLength s = do (x,y) <- size
                   return (scale s ((x+y)/2))

scaleWidth :: Scale -> Canvas Float
scaleWidth s = do (x,_) <- size
                  return (scale s x)

scaleHeight :: Scale -> Canvas Float
scaleHeight s = do (_,y) <- size
                   return (scale s y)

scaleX :: Scale -> Canvas XCo
scaleX s = do (x,_) <- size
              return (scale s x)

scaleY :: Scale -> Canvas YCo
scaleY s = do (_,y) <- size
              return (scale (1-s) y)

toscale :: Int -> Float -> Scale
toscale i z = realToFrac (fromIntegral i / z)

toscaleX :: Int -> Canvas Scale
toscaleX i = do (x,_) <- size
                return (toscale i x)

toscaleY :: Int -> Canvas Scale
toscaleY i = do (_,y) <- size
                return (1 - toscale i y)

runSFcanvas :: IO a -> SF a b -> (b -> Canvas ()) -> Context -> IO ()
runSFcanvas inp sf r canvas = do t0  <- getCurrentTime
                                 ref <- newIORef t0
                                 reactimate inp
                                            (\_ -> do dt <- getTick ref
                                                      a  <- inp
                                                      return (dt,Just a))
                                            (\ _ b -> send canvas (render (r b)) >> return False)
                                            sf

getTick :: IORef UTCTime -> IO DTime
getTick x = do t0 <- readIORef x
               t1 <- getCurrentTime
               writeIORef x t1
               return (realToFrac (diffUTCTime t1 t0))

render :: Canvas a -> Canvas ()
render r = do (width,height) <- size
              clearRect (0,0,width,height)
              beginPath ()
              save ()
              r
              restore ()
