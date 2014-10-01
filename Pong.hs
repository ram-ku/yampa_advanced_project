{-# LANGUAGE Arrows, ScopedTypeVariables #-}

module Pong where

import Data.List (sort)

import FRP.Yampa
import FRP.Yampa.Geometry

import Ball (g,elasticity,switchWhen,iIntegral,
             Radius,Position,Velocity)
import Balls

-------------------------------------------------------------------------

-- Paddles

type Height = Double
type Width  = Double
type Speed  = Double

data Direction = MoveLeft | Halt | MoveRight deriving Eq

data Paddle = Paddle
                { padLeftEdge :: Position
                , padTopEdge  :: Position
                , padHeight   :: Height
                , padWidth    :: Width
                , padSpeed    :: Speed
                , padCol      :: Colour
                }

data Spaceship = Spaceship
			{ shipLeftEdge :: Position
                , shipTopEdge  :: Position
                , shipHeight   :: Height
                , shipWidth    :: Width
                , shipSpeed    :: Speed
                , shipCol      :: Colour
                }


padRightEdge :: Paddle -> Position
padRightEdge p = padLeftEdge p + padWidth p

padBotEdge :: Paddle -> Position
padBotEdge p = padTopEdge p - padHeight p

padCentrePoint :: Paddle -> Position
padCentrePoint p = padLeftEdge p + padWidth p / 2


shipRightEdge :: Spaceship -> Position
shipRightEdge p = shipLeftEdge p + shipWidth p

shipBotEdge :: Spaceship -> Position
shipBotEdge p = shipTopEdge p - shipHeight p

shipCentrePoint :: Spaceship -> Position
shipCentrePoint p = shipLeftEdge p + shipWidth p / 2
-------------------------------------------------------------------------

-- Paddle behaviour

-- convert a direction to velocity, halting at walls
dirToVel :: Direction -> Paddle -> Velocity
dirToVel Halt      _ = 0
dirToVel MoveLeft  p = if padLeftEdge p > 0 then negate (padSpeed p) else 0
dirToVel MoveRight p = if padRightEdge p < 1 then padSpeed p else 0

paddle :: Paddle -> SF Direction Paddle
paddle p0 = let x0 = padLeftEdge p0
             in
                proc dir -> do
                  rec
                      p1 <- iPre p0 -< p
                      x <- iIntegral x0 -< dirToVel dir p1
                      let p = p1 {padLeftEdge = x}
                  returnA -< p

-------------------------------------------------------------------------

-- Ball/Paddle interaction

-- is the ball vertically aligned with the paddle
verticalHit :: Paddle -> Ball2 -> Bool
verticalHit p b =    ballPosX b + ballRad b > padLeftEdge p
                  && ballPosX b - ballRad b < padRightEdge p

-- is the ball horizontally aligned with the paddle
horizontalHit :: Paddle -> Ball2 -> Bool
horizontalHit p b =    ballPosY b - ballRad b < padTopEdge p
                    && ballPosY b + ballRad b > padBotEdge p

padTopBounce :: BounceDetector (Ball2, Paddle)
padTopBounce = proc (b,p) -> do
                 e <- edgeTag ybounce -< ballPosY b - ballRad b < padTopEdge p
                 returnA              -< gate e (verticalHit p b)

padLeftBounce :: BounceDetector (Ball2, Paddle)
padLeftBounce = proc (b,p) -> do
                  e <- edge -< ballPosX b + ballRad b > padLeftEdge p
                  returnA   -< tagWith (modVel p) (gate e (horizontalHit p b))
  where
        modVel p v = if vector2X v > 0 then xbounce v else modify2X (\ x -> x - padSpeed p) v

padRightBounce :: BounceDetector (Ball2, Paddle)
padRightBounce = proc (b,p) -> do
                   e <- edge -< ballPosX b - ballRad b < padRightEdge p
                   returnA   -< tagWith (modVel p) (gate e (horizontalHit p b))
  where
        modVel p v = if vector2X v < 0 then xbounce v else modify2X (\ x -> x + padSpeed p) v


paddleDetector :: BounceDetector (Ball2,Paddle)
paddleDetector = mergeDetectors [padTopBounce, padLeftBounce, padRightBounce]
--------------------------------------------------------------------------------------
--Spaceship Interaction code---
verticalHit' :: Spaceship -> Ball2 -> Bool
verticalHit' p b =    ballPosX b + ballRad b > shipLeftEdge p
                  && ballPosX b - ballRad b < shipRightEdge p

-- is the ball horizontally aligned with the paddle
horizontalHit' :: Spaceship -> Ball2 -> Bool
horizontalHit' p b =    ballPosY b - ballRad b < shipTopEdge p
                    && ballPosY b + ballRad b > shipBotEdge p

shipTopBounce :: BounceDetector (Ball2, Spaceship)
shipTopBounce = proc (b,p) -> do
                 e <- edgeTag ybounce -< ballPosY b - ballRad b < shipTopEdge p
                 returnA              -< gate e (verticalHit' p b)

shipLeftBounce :: BounceDetector (Ball2, Spaceship)
shipLeftBounce = proc (b,p) -> do
                  e <- edge -< ballPosX b + ballRad b > shipLeftEdge p
                  returnA   -< tagWith (modVel p) (gate e (horizontalHit' p b))
  where
        modVel p v = if vector2X v > 0 then xbounce v else modify2X (\ x -> x - shipSpeed p) v

shipRightBounce :: BounceDetector (Ball2, Spaceship)
shipRightBounce = proc (b,p) -> do
                   e <- edge -< ballPosX b - ballRad b < shipRightEdge p
                   returnA   -< tagWith (modVel p) (gate e (horizontalHit' p b))
  where
        modVel p v = if vector2X v < 0 then xbounce v else modify2X (\ x -> x + shipSpeed p) v

shipBottomBounce :: BounceDetector (Ball2, Spaceship)
shipBottomBounce = proc (b,p) -> do
                 e <- edgeTag ybounce -< ballPosY b + ballRad b > shipBotEdge p
                 returnA              -< gate e (verticalHit' p b)


spaceshipDetector ::  BounceDetector ( Ball2,Spaceship)
spaceshipDetector = mergeDetectors [shipTopBounce,shipLeftBounce,shipRightBounce,shipBottomBounce]

-------------------------------------------------------------------------

-- Balls that can drop off the screen.
-- The essense of this is that once they're out of sight we want them to be deleted.

-- As well as outputing the Ball2 state, it also emits an event when it drops out of sight.
-- That is, each ball effectively announces when it should be deleted.
-- We make the input signal a paramater so that this can be reused when aliens are added.
type LoseableBall a = SF a (Ball2, Event ())

detectBounces :: BounceDetector (Ball2,a) -> SF (Ball2,a) (Event Ball2)
detectBounces det = proc (ball,a) -> do
                      e <- det -< (ball,a)
                      returnA  -< updateBallVel ball e

loseableBall :: forall a. BounceDetector (Ball2,a) -> Ball2 -> LoseableBall a
loseableBall det b0 =
                     proc a -> do
                       b      <- bouncingBallAux b0 -< a
                       lostEv <- edge               -< ballPosY b < negate (ballRad b)
                       returnA                      -< (b,lostEv)
  where
        bouncingBallAux :: Ball2 -> SF a Ball2
        bouncingBallAux b0 = switch (proc a -> do
                                       b <- fallingBall2 b0   -< a
                                       e <- detectBounces det -< (b,a)
                                       returnA                -< (b,e)
                                    )
                                    bouncingBallAux

loseableNewBalls :: forall a. BounceDetector (Ball2,a) -> [Ball2] -> SF (Event Ball2, a) [Ball2]
loseableNewBalls det bs = newBallsAux (map (loseableBall det) bs) >>^ map fst
  where
    newBallsAux :: [LoseableBall a] -> SF (Event Ball2, a) [(Ball2,Event ())]
    newBallsAux sfs = pSwitch
                         (\ (_,a) sfs' -> [ (a,sf) | sf <- sfs'] )
                         sfs
                         (proc ((eb,_),bes) -> do
                            eb' <- notYet -< eb
                            returnA       -< eventGen eb' (map snd bes)
                         )
                         updateCollection

    eventGen :: Event Ball2 -> [Event ()] -> Event (Maybe (LoseableBall a), [Int])
    eventGen eb es = mapMerge (\ b    -> (Just b, []))
                              (\ ns   -> (Nothing,  ns))
                              (\ b ns -> (Just b, ns))
                              (fmap (loseableBall det) eb)
                              (catEvents [ tagWith n e | (n,e) <- zip [0..] es ])

    updateCollection :: [LoseableBall a] -> (Maybe (LoseableBall a), [Int]) -> SF (Event Ball2, a) [(Ball2,Event ())]
    updateCollection sfs' (mb,xs) = newBallsAux (consMaybe mb $ removeIndices xs sfs')

-- pSwitch :: (forall sf. a -> [sf] -> [(b,sf)]) -- route the input to the signal functions in the collection
--         -> [SF b c]                           -- the initial collection of signal functions
--         -> SF (a,[c]) (Event e)               -- generate the switching event based on the overall input and outputs
--         -> [SF b c] -> e -> SF a [c]          -- generate a signal function to replace *the entire collection*,
--                                               -- using the existing signal functions and the event that triggered the switch
--         -> SF a [c]

------------------------------------------------------------------------

-- Utilities

consMaybe :: Maybe a -> [a] -> [a]
consMaybe Nothing  as = as
consMaybe (Just a) as = a : as

removeIndices :: [Int] -> [a] -> [a]
removeIndices ns as = [ a | (n,a) <- zip [0..] as, n `notElem` ns ]

-------------------------------------------------------------------------

type PongBall = LoseableBall Paddle

wallsAndCeilingDetector :: BounceDetector Ball2
wallsAndCeilingDetector = mergeDetectors [ceilingBounce, wallBounceL, wallBounceR]

--pongDetector :: BounceDetector (Ball2,(Paddle,Spaceship))
pongDetector = mergeDetectors [paddleDetector,fst ^>> wallsAndCeilingDetector]

pongBall :: Ball2 -> PongBall
pongBall = loseableBall pongDetector

pongBalls :: [Ball2] -> SF (Event Ball2, (Paddle,Spaceship)) [Ball2]
pongBalls = loseableNewBalls newDetector

wallsAndCeilingDetector' :: BounceDetector (Ball2,(Paddle,Spaceship))
wallsAndCeilingDetector' = proc (b,(p,s)) -> do {
											 res <- wallsAndCeilingDetector -< b;
											 returnA -< res}

paddleDetector' :: BounceDetector (Ball2,(Paddle,Spaceship))
paddleDetector' = proc (b,(p,s)) -> do {
										 res <- paddleDetector -<(b,p);
										 returnA -< res}

spaceshipDetector' :: BounceDetector (Ball2,(Paddle,Spaceship))
--spaceshipDetector' = mergeDetectors [shipTopBounce,shipLeftBounce,shipRightBounce]
spaceshipDetector'=proc (b,(p,s)) -> do {
										 res <- spaceshipDetector -<(b,s);
										 returnA -< res}

newDetector::BounceDetector (Ball2,(Paddle,Spaceship))
newDetector = mergeDetectors [paddleDetector',wallsAndCeilingDetector',spaceshipDetector']

-------------------------------------------------------------------------


spaceshipSF :: Spaceship -> SF (Event Position2) (Spaceship, Event Position2)

spaceshipSF s = switch (waitShip s &&& identity) (\ epos -> switch (movingship s &&& never ) (\ sp -> spaceshipSF sp))

	where 
		waitShip:: Spaceship -> SF (Event Position2)(Spaceship,Event Position2)
		waitShip s = constant s &&& (identity >>> repeatedly 0.5 ((vector2 ((shipLeftEdge s)+0.025) ((shipTopEdge s)+0.03))::Position2))

modifyvecx x0 s _ (Event (v::Position2)) = if (i < x0) &&(n == -1) then n else if (i < x0) && (n==1) then -1*n else if (i > x0)&& (n==1) then n else -1*n
										where {(i,j)= vector2XY v;
											   n = if (s == 0) then 1 else s}

modifyvecx x0 s (i,j) noEvent=if abs (x0-i) < 0.01 then 0 else s
										
modifyvecy y0 s _ (Event (v::Position2)) = if (j < y0) &&(n == -1) then n else if (j < y0) && (n==1) then -1*n else if (j > y0)&& (n==1) then n else -1*n
										where {(i,j)= vector2XY v;
												n =  if (s==0) then 1 else s}
modifyvecy y0 s (i,j) noEvent=if abs (y0-j) < 0.01 then 0 else s

saveepos savedepos (Event (v::Position2))= vector2XY v
saveepos savedepos NoEvent= savedepos

movingship :: Spaceship -> SF (Event Position2) (Spaceship, Event Position2)
movingship s = let {x0 = shipLeftEdge s;
				    y0 = shipTopEdge s}
                  in
                    proc epos -> do{
                      
					  rec {savedepos<- iPre (1,1)  -< saveepos savedepos epos;
					  spfactorx <- iPre 1 -< modifyvecx x spfactorx savedepos epos;
					  x <- iIntegral x0 -< spfactorx * (shipSpeed s)};
					  rec {spfactory <- iPre 1 -< modifyvecy y spfactory savedepos epos;
					  y <- iIntegral y0 -< spfactory * (shipSpeed s)};
					  evnt <- repeatedly 0.5 ((vector2 1 ((shipTopEdge s)-0.03))::Position2) &&& identity -< Event (vector2 (x+0.025) (y-0.03) :: Position2);
                      returnA           -< (s {shipLeftEdge = x,shipTopEdge = y}, tmp1 evnt)}
                   
-------------------------------------------------------------------------------
-- Take each Position2 event in the input and convert it to a Ball2.
-- Also cycles through a stream of colours so that each ball has a different colour.

newBallCol :: [Colour] -> SF (Event Position2) (Event Ball2)
newBallCol (col : cols) = dSwitch (proc epos -> do
                                     epos' <- notYet -< epos
                                     returnA         -< (fmap (newBall col) epos, epos')
                                  )
                                  (\ _ -> newBallCol cols)

newBall :: Colour -> Position2 -> Ball2
newBall col pos = Ball2 newRad pos newVel col
  where
    newVel :: Velocity2
    newVel =  vector2 0.3 0.3

    newRad :: Radius
    newRad =  0.025

newCols :: [Colour]
newCols =  cycle ["purple","maroon","magenta","cyan","violet","pink","black","brown","grey","red","orange","yellow","green","blue"]

-------------------------------------------------------------------------

tmp (NoEvent,NoEvent) = noEvent
tmp (NoEvent, a::Event Position2) = a
tmp (a::Event Position2,NoEvent) = a
tmp (a::Event Position2,b::Event Position2) = a

tmp1 (NoEvent,NoEvent) = noEvent
tmp1 (NoEvent, a::Event Position2) = NoEvent
tmp1 (a::Event Position2,NoEvent) = a
tmp1 (a::Event Position2,b::Event Position2) = b
		
tmp2 (NoEvent,NoEvent) = noEvent
tmp2 (NoEvent, a::Event Position2) = a
tmp2 (a::Event Position2,NoEvent) = a
tmp2 (a::Event Position2,b::Event Position2) = b


type PaddleAI = SF ([Ball2],Paddle) Direction
-- The main game model.
				   
pongSF :: [Ball2] -> Spaceship -> Paddle -> PaddleAI -> Spaceship -> SF (Event Position2) ([Ball2],Paddle,Spaceship,Spaceship)
pongSF bs0 sp pad0 ai sp2= proc epos -> do {
		       (ship,aa)<- spaceshipSF sp		  -< epos;
		       (ship2,bb) <- spaceshipSF sp2<<< delayEvent 1 <<< iPre NoEvent<<<iPre NoEvent <<< iPre NoEvent	  -< aa;
			   evnt <- identity -< (epos,aa);
		        eba  <- newBallCol newCols 	 -< (tmp2 (tmp (epos,aa),bb));
                       rec {bs   <- pongBalls bs0                          -< (eba,(pad,ship));
                            pad  <- paddle pad0 <<< ai <<< iPre (bs0,pad0) -< (bs,pad)};
				   returnA                                            -< (bs,pad,ship,ship2)}

-------------------------------------------------------------------------

pongExample :: SF (Event Position2) ([Ball2],Paddle,Spaceship,Spaceship)
pongExample = pongSF defaultPongBalls defaultShip defaultPaddle steadyPlannerAI defaultShip2

-------------------------------------------------------------------------

defaultPongBalls :: [Ball2]
defaultPongBalls =
         [ Ball2 0.01 (vector2 0.2 0.75) (vector2 0.2    0.5   ) "red"
         , Ball2 0.02 (vector2 0.3 0.95) (vector2 0.15   0.0   ) "orange"
         , Ball2 0.03 (vector2 0.5 0.65) (vector2 (-0.6) (-0.4)) "yellow"
         , Ball2 0.04 (vector2 0.6 0.85) (vector2 (-0.1) 0.2   ) "green"
         , Ball2 0.05 (vector2 0.4 0.6)  (vector2 0.15   0.7   ) "blue"
         ]

defaultPaddle :: Paddle
defaultPaddle = Paddle { padLeftEdge = 0.45
                       , padTopEdge  = 0.1
                       , padHeight   = 0.015
                       , padWidth    = 0.1
                       , padSpeed    = 0.6
                       , padCol      = "black"
                       }


defaultShip = Spaceship { shipLeftEdge = 0.4
                       , shipTopEdge  = 0.7
                       , shipHeight   = 0.06
                       , shipWidth    = 0.05
                       , shipSpeed    = 0.05
                       , shipCol      = "red"
                       }
defaultShip2 = Spaceship { shipLeftEdge = 0.1
			, shipTopEdge  = 0.6
                       , shipHeight   = 0.05
                       , shipWidth    = 0.04
                       , shipSpeed    = 0.02
                       , shipCol      = "blue"
                       }
-------------------------------------------------------------------------

-- AI decision making (unimportant, don't read)

arrivalTime :: Paddle -> Ball2 -> Time
arrivalTime pad ball = let u = ballVelY ball
                       in ( (-u) - sqrt (u*u - 2 * g * (ballPosY ball - padTopEdge pad)) ) / g

arrivalPos :: Ball2 -> Time -> Position
arrivalPos b t = go ((ballVelX b * t) + ballPosX b)
                 where r = ballRad b
                       go x | x < r       = go (2*r - x)
                            | x > (1 - r) = go (2 * (1-r) - x)
                            | otherwise   = x

inPlay :: Paddle -> Ball2 -> Bool
inPlay p b = ballPosY b > padHeight p + ballRad b

reachable :: Paddle -> Time -> Position -> Ball2 -> Bool
reachable pad t pos b =    padLeftEdge  pad - t * padSpeed pad - ballRad b - marginOfError < pos
                        && padRightEdge pad + t * padSpeed pad + ballRad b + marginOfError > pos
  where marginOfError = 0.04


targets :: Paddle -> [Ball2] -> [Position]
targets pad bs = map snd $ sort [ (t,pos) | b <- bs, inPlay pad b, let t = arrivalTime pad b, let pos = arrivalPos b t, reachable pad t pos b]

moveTo :: Position -> Paddle -> Direction
moveTo pos pad = if padCentrePoint pad < pos then MoveRight else MoveLeft

{-
moveAndHaltShip=switch (waitShip &&& clickevent)(\ _ -> switch (approachclick &&& reachedevent)(\ _ -> moveAndHaltShip))
	where
		
		waitShip = constant s &&& identity
-}
		

moveToAndHalt :: SF (Position,Paddle) Direction
moveToAndHalt = switch (approach &&& closeEnough 0.05) (\ _ -> switch (wait &&& notCloseEnough 0.1) (\ _ -> moveToAndHalt))
   where

         approach :: SF (Position,Paddle) Direction
         approach = arr (uncurry moveTo)

         wait :: SF (Position,Paddle) Direction
         wait = constant Halt

         thisClose :: Double -> (Position,Paddle) -> Bool
         thisClose z (pos,pad) =    (pos > padCentrePoint pad - z * padWidth pad)
                                 && (pos < padCentrePoint pad + z * padWidth pad)

         closeEnough :: Double -> SF (Position,Paddle) (Event ())
         closeEnough z = thisClose z ^>> iEdge False

         notCloseEnough :: Double -> SF (Position,Paddle) (Event ())
         notCloseEnough z = (not . thisClose z) ^>> iEdge False

-- Position the paddle directly under where the next ball will pass the baseline (if you can reach it).
greedyAI :: PaddleAI
greedyAI = arr $ \ (bs,pad) -> case targets pad bs of
                                 []        -> Halt
                                 (pos : _) -> moveTo pos pad

planPosition :: Paddle -> Position -> Position -> Position
planPosition pad pos1 pos2 = if pos2 > pos1
                              then pos1 + (padWidth pad * 0.2)
                              else pos1 - (padWidth pad * 0.2)

-- As greedyAI, but position the paddle slightly to one side to be closer to the second ball.
planAheadAI :: PaddleAI
planAheadAI = arr $ \ (bs,pad) -> case targets pad bs of
                                    []                -> Halt
                                    [pos]             -> moveTo pos pad
                                    (pos1 : pos2 : _) -> moveTo (planPosition pad pos1 pos2) pad

-- As planAheadAI, but halt when in position rather than fidgeting.
steadyPlannerAI :: PaddleAI
steadyPlannerAI = proc (bs,pad) -> do
                    let pos = case targets pad bs of
                                []                -> padCentrePoint pad
                                [pos0]            -> pos0
                                (pos1 : pos2 : _) -> planPosition pad pos1 pos2
                    moveToAndHalt -< (pos,pad)
