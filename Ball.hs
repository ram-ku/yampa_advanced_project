{-# LANGUAGE Arrows #-}

module Ball where

import FRP.Yampa

-------------------------------------------------------------------------

-- Types

type Acceleration = Double
type Velocity     = Double
type Position     = Double
type Radius       = Double

data Ball         = Ball { ballRad  :: Radius
                         , ballPosX :: Position
                         , ballPosY :: Position
                         , ballVel  :: Velocity
                         }

-------------------------------------------------------------------------

-- Constants

g :: Acceleration
g = -1

elasticity :: Double
elasticity = -0.99

-------------------------------------------------------------------------

-- Bouncing Ball

fallingBall :: Ball -> SF a Ball
fallingBall b = let v0 = ballVel b
                    y0 = ballPosY b
                 in
                    proc _ -> do
                      v <- iIntegral v0 -< g
                      y <- iIntegral y0 -< v
                      returnA           -< b { ballVel = v, ballPosY = y }

detectBounce :: SF Ball (Event Ball)
detectBounce = proc b -> do
                 e <- edge  -<  ballPosY b <= ballRad b
                 returnA    -<  tag e (b {ballVel = ballVel b * elasticity})

bouncingBall :: Ball -> SF a Ball
bouncingBall b = switchWhen (fallingBall b) detectBounce bouncingBall

-------------------------------------------------------------------------

ball1 :: Ball
ball1 = Ball { ballRad  = 0.04
             , ballPosX = 0.5
             , ballPosY = 0.8
             , ballVel  = 0
             }

bouncingBallExample :: SF a Ball
bouncingBallExample = bouncingBall ball1

-------------------------------------------------------------------------

-- Utilities

switchWhen :: SF a b -> SF b (Event e) -> (e -> SF a b) -> SF a b
switchWhen sf sfe = switch (sf >>> (identity &&& sfe))

iIntegral :: VectorSpace x s => x -> SF x x
iIntegral x = integral >>> arr (^+^ x)

-------------------------------------------------------------------------
