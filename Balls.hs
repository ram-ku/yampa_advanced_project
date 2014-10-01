{-# LANGUAGE Arrows #-}

module Balls where

import FRP.Yampa
import FRP.Yampa.Geometry

import Ball (g,elasticity,switchWhen,iIntegral,
             Radius,Position,Velocity,Acceleration)

-------------------------------------------------------------------------

-- 2D Bouncing Balls

type Acceleration2 = Vector2 Acceleration
type Velocity2     = Vector2 Velocity
type Position2     = Vector2 Position
type Colour        = String
data Ball2         = Ball2 { ballRad :: Radius
                           , ballPos :: Position2
                           , ballVel :: Velocity2
                           , ballCol :: Colour
                           }

ballPosX :: Ball2 -> Position
ballPosX = vector2X . ballPos

ballPosY :: Ball2 -> Position
ballPosY = vector2Y . ballPos

ballVelX :: Ball2 -> Position
ballVelX = vector2X . ballVel

ballVelY :: Ball2 -> Position
ballVelY = vector2Y . ballVel

-------------------------------------------------------------------------

g2 :: Acceleration2
g2 = vector2 0 g

-------------------------------------------------------------------------

fallingBall2 :: Ball2 -> SF a Ball2
fallingBall2 b = proc _ -> do
                   v <- iIntegral (ballVel b) -< g2
                   p <- iIntegral (ballPos b) -< v
                   returnA                    -< b {ballVel = v, ballPos = p}

-------------------------------------------------------------------------

type BounceDetector a = SF a (Event (Velocity2 -> Velocity2))


mergeDetectors :: [BounceDetector a] -> BounceDetector a
mergeDetectors detectors = parB detectors >>> arr combineVelUpdates
  where
    combineVelUpdates :: [Event (Velocity2 -> Velocity2)] -> Event (Velocity2 -> Velocity2)
    combineVelUpdates = fmap (foldr (.) id) . catEvents


xbounce :: Velocity2 -> Velocity2
xbounce = modify2X (* elasticity)

ybounce :: Velocity2 -> Velocity2
ybounce = modify2Y (* elasticity)


-- SF Ball2 (Event (Velocity2 -> Velocity2))
floorBounce :: BounceDetector Ball2
floorBounce = proc b -> do
                edgeTag ybounce -< (ballPosY b <= ballRad b)

ceilingBounce :: BounceDetector Ball2
ceilingBounce = proc b -> do
                  edgeTag ybounce -< (ballPosY b + ballRad b >= 1)

wallBounceL :: BounceDetector Ball2
wallBounceL = proc b -> do
                edgeTag xbounce -< (ballPosX b <= ballRad b)

wallBounceR :: BounceDetector Ball2
wallBounceR = proc b -> do
                edgeTag xbounce -< (ballPosX b + ballRad b >= 1)

-------------------------------------------------------------------------

-- It would be better (for extensibility) to abstract out the BounceDetectors from the following.
-- I have not done so for clarity.
-- See Pong.

updateBallVel :: Ball2 -> Event (Velocity2 -> Velocity2) -> Event Ball2
updateBallVel b = fmap (\ f -> b { ballVel = f (ballVel b) })

detectBounce2 :: SF Ball2 (Event Ball2)
detectBounce2 = proc b -> do
                  e <- mergeDetectors [floorBounce, ceilingBounce, wallBounceL, wallBounceR] -< b
                  returnA -< updateBallVel b e

bouncingBall2 :: Ball2 -> SF a Ball2
bouncingBall2 b = switchWhen (fallingBall2 b) detectBounce2 bouncingBall2

-------------------------------------------------------------------------

bouncingBalls :: [Ball2] -> SF a [Ball2]
bouncingBalls bs = parB (map bouncingBall2 bs)

-------------------------------------------------------------------------

defaultBalls :: [Ball2]
defaultBalls =
         [ Ball2 0.01 (vector2 0.2 0.7) (vector2 0.4     1.5   ) "red"
         , Ball2 0.02 (vector2 0.3 0.9) (vector2 0.3     (-0.9)) "orange"
         , Ball2 0.03 (vector2 0.5 0.6) (vector2 (-1.2)  (-0.8)) "yellow"
         , Ball2 0.04 (vector2 0.6 0.3) (vector2 (-0.15) 0.2   ) "green"
         , Ball2 0.05 (vector2 0.4 0.7) (vector2 0.25    0.1   ) "blue"
         ]

-------------------------------------------------------------------------

-- Dynamically adding new balls

-- pSwitch :: (forall sf. a -> [sf] -> [(b,sf)]) -- route the input to the signal functions in the collection
--         -> [SF b c]                           -- the initial collection of signal functions
--         -> SF (a,[c]) (Event e)               -- generate the switching event based on the overall input and outputs
--         -> [SF b c] -> e -> SF a [c]          -- generate a signal function to replace *the entire collection*,
--                                               -- using the exisiting signal functions and the event that triggered the switch
--         -> SF a [c]

newBalls :: [Ball2] -> SF (Event Position2) [Ball2]
newBalls bs = newBallsAux (map bouncingBall2 bs)
  where
    newBallsAux :: [SF () Ball2] -> SF (Event Position2) [Ball2]
    newBallsAux sfs =  pSwitch
                          (\ _ sfs' -> [ ((),sf) | sf <- sfs'] )
                          sfs
                          --(arr fst >>> notYet)
					 (repeatedly 0.5 (vector2 0.5 0.5))
					 --(arr noEvent)
                          (\ sfs' pos -> newBallsAux (newBallSF pos : sfs'))

    newBallSF :: Position2 -> SF a Ball2
    newBallSF pos = bouncingBall2 (Ball2 newRad pos newVel newCol) >>> identity
    --newBallSF pos = switch (bouncingBall2 (Ball2 newRad pos newVel newCol) >>> (identity &&& now (vector2 0.5 0.5))) (bouncingBall2 (Ball2 newRad (vector2 0.5 0.5) newVel newCol))

    newVel :: Velocity2
    newVel =  vector2 0 0

    newRad :: Radius
    newRad =  0.025

    newCol :: Colour
    newCol = "purple"

-------------------------------------------------------------------------

newBallsExample :: SF (Event Position2) [Ball2]
newBallsExample = newBalls defaultBalls

-------------------------------------------------------------------------

-- Utilities

modify2X :: RealFloat a => (a -> a) -> Vector2 a -> Vector2 a
modify2X f v = vector2 (f x) y
                 where (x,y) = vector2XY v

modify2Y :: RealFloat a => (a -> a) -> Vector2 a -> Vector2 a
modify2Y f v = vector2 x (f y)
                 where (x,y) = vector2XY v

-------------------------------------------------------------------------
