{-
  Simple Asteroids-like game using the Gloss library
  Pedro Vasconcelos, 2014--2017
-}

module Main where

import System.Random (randomRIO)
import Control.Monad (forM) 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | representation for the game state;
-- player's ship plus list of asteroids and lasers
data GameState = GameState Entity [Entity]

-- | an entity in space;
-- pair of visual shape and movement coordinates
type Entity = (Shape, Coords)

-- | shapes of diferent entities
data Shape = Asteroid Float   -- ^ asteroids (of given size)
           | Laser Float      -- ^ lasers (time to decay)
           | Ship             -- ^ player's ship


-- | coordinates for movement calculations
-- position, velocity vector, heading, angular velocity
type Coords = (Point,      -- ^ center position
               Vector,     -- ^ linear velocity
               Float,      -- ^ heading (degrees)
               Float       -- ^ angular velocity (degrees/s)
              )


-- | test if a entity is an asteroid
isAsteroid :: Entity -> Bool
isAsteroid (Asteroid _, _) = True
isAsteroid _               =  False

-- | test if an entity if a laser beam
isLaser :: Entity -> Bool
isLaser (Laser _, _) = True
isLaser _            = False


-- | render all entities into a picture
render :: GameState -> Picture
render (GameState ship other) = pictures (map renderEnt (ship:other))


-- | render a single entity
renderEnt :: Entity -> Picture
renderEnt (shape, ((x,y), _, ang, _))
  = translate x y $ rotate ang $ renderShape shape


-- | render a shape in origin and with default heading
renderShape :: Shape -> Picture
renderShape Ship            = color green ship
renderShape (Laser time)    = color yellow laser
renderShape (Asteroid size) = color red (scale size size asteroid)

-- | basic shapes
ship, laser, asteroid :: Picture
ship = polygon [ (20,0), (-10,10), (-5,0), (-10,-10), (20,0)]
laser = line [(0,0), (10,0)]
asteroid = polygon [ (-10,5), (-5, 10)
                   , (10,5), (5, -5)
                   , (-5,-10), (-10,5)
                   ]


-- | advance coordinates by a time delta
advanceCoords :: Float -> Coords -> Coords
advanceCoords dt ((x,y), (dx,dy), ang, angV)
  = ((x',y'), (dx,dy), ang', angV)
    where x' = wrap (x+dt*dx) maxWidth
          y' = wrap (y+dt*dy) maxHeight
          ang' = ang + dt*angV
          wrap h max | h > max = h-2*max
                     | h < -max= h+2*max
                     | otherwise = h

-- | advance an entity by a time delta
advanceEnt :: Float -> Entity -> Entity
advanceEnt dt (shape, mov) = (shape, advanceCoords dt mov)


-- | advance all game entities by a time delta
advance :: Float -> GameState -> GameState
advance dt (GameState ship other)
  = GameState (advanceEnt dt ship) (map (advanceEnt dt) other)

-- | update the game state;
-- time delta, decay laser and check for colitions
update :: Float -> GameState -> GameState
update dt
  = collisions . decay dt . advance dt


-- | decay lasers
decay :: Float -> GameState -> GameState
decay dt (GameState ship objs)
  = GameState ship (filter remain (map decr objs))
  where
    -- decrement time left for each laser
    decr (Laser t, mov)   = (Laser (t-dt), mov)
    decr obj               = obj
    -- test remaining lasers (positive time left)
    remain (Laser t, mov) = t>0   -- "lasers" só com tempo positivo
    remain _              = True  -- tudo o resto permanece


-- | collision detection
collisions :: GameState -> GameState
collisions (GameState ship objs)
  =  GameState ship (frags ++ objs' ++ objs'')
  where rocks  = filter isAsteroid objs  -- all asteroids
        lasers = filter isLaser objs     -- all lasers
        frags = concat [fragment rock | rock<-rocks,
                        any (`hits`rock) lasers]
        objs' = [obj | obj<-rocks,
                 not (any (`hits`obj) lasers)]
        objs''= [obj | obj<-lasers,
                 not (any (obj`hits`) rocks)]


-- | check colision between two entities;
-- between lasers and asteriods only for simplicity
hits :: Entity -> Entity -> Bool
hits (Laser _, ((x,y), _, _, _)) (Asteroid sz, ((x',y'), _, _, _))
  = (x-x')**2 + (y-y')**2 <= (sz*10)**2
hits _ _ = False


-- | fragment an asteroid into 4 smaller ones
-- remove too small fragments
fragment :: Entity -> [Entity]
fragment (Asteroid sz, (pt, (dx,dy), ang, angV))
  | sz'>= 1  = [(Asteroid sz', (pt, vel', ang, angV)) | vel'<- vels]
  | otherwise= []
  where sz'  = 0.5*sz
        vels = [(dy,dx),(-dx,dy),(dx,-dy),(-dy,-dx)]


-- | react to keyboard events
react :: Event -> GameState -> GameState
-- rotate ship (left/right)
react (EventKey (SpecialKey KeyLeft) keystate _ _) (GameState ship objs)
  = GameState ship' objs
  where  (Ship, (pos, vel, ang, angV)) = ship
         angV' = if keystate==Down then (-180) else 0
         ship' = (Ship, (pos, vel, ang, angV'))

react (EventKey (SpecialKey KeyRight) keystate _ _) (GameState ship objs)
  = GameState ship' objs
  where  (Ship, (pos, vel, ang, angV)) = ship
         angV'= if keystate==Down then 180 else 0
         ship' = (Ship, (pos, vel, ang, angV'))

-- acelerate forward
react (EventKey (SpecialKey KeyUp) Down _ _) (GameState ship objs)
  = GameState ship' objs
  where (Ship, (pos, (dx,dy), ang, angV)) = ship
        dx' = dx + 10*cos (-ang/180*pi)
        dy' = dy + 10*sin (-ang/180*pi)
        ship' = (Ship, (pos, (dx',dy'), ang, angV))

-- fire a new laser
react (EventKey (SpecialKey KeySpace) Down _ _) (GameState ship objs)
  = GameState ship (proj:objs)
  where (Ship, (pos, _, ang, _)) = ship
        vel = (400*cos (-ang/180*pi), 400*sin (-ang/180*pi))
        proj = (Laser 1, (pos, vel, ang, 0))  -- decai após 1 seg.

-- ignore all other keys and events
react _ world = world


-- | generate a random asteroid
randAsteroid :: IO Entity
randAsteroid = do
  pos <- randPoint
  vel <- randVel
  angV <- randomRIO (-90,90)
  ang <- randomRIO (-180,180)
  sz <- randomRIO (1,4)
  return (Asteroid sz, (pos,vel,ang,angV))

randVel :: IO Vector
randVel = do
  dx <- randomRIO (-50,50)
  dy <- randomRIO (-50,50)
  return (dx,dy)

randPoint :: IO Point
randPoint = do
  x <- randomRIO (-maxWidth,maxWidth)
  y <- randomRIO (-maxHeight,maxHeight)
  return (x,y)


-- | display dimensions
maxWidth, maxHeight :: Float
maxWidth = 300
maxHeight = 300

-- | frames per second for game event loop
fps :: Int
fps = 60

-- | initial game state
initialState :: [Entity] -> GameState
initialState asteroids = GameState ship asteroids
  where
    ship = (Ship, ((0,0), (0,0), 0, 0)) -- origin (screen center)

-- | main entry pont
main :: IO ()
main = do
  asteroids <- forM [1..10] (\_ -> randAsteroid)
  play window black fps (initialState asteroids) render react update

window :: Display
window = InWindow "Asteroids" (2*round maxWidth,2*round maxHeight) (0,0)
