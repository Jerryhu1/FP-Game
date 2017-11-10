module Model.GameState where

import System.Random
import Graphics.Gloss.Game

import Model.Typeclasses.Renderizable

import Model.Player
import Model.Grid
import Model.GameObject
import Model.Typeclasses.Positioned
import Debug.Trace

data GameState = GameState {
    player       :: Player,
    grid         :: Grid,
    currentState :: CurrentState,
    gen          :: StdGen,
    keyState     :: KeyState,
    enemies      :: Enemies,
    bombs        :: Bombs,
    explosions   :: [Explosion]
}

type Enemies = [Player]

instance Renderizable GameState where
    render gs
              | currentState gs == Paused   = pictures (pics ++ [png "res/pause-screen.png"] )
              | currentState gs == GameOver = pictures (pics ++ [png "res/lose-screen.png"])
              | currentState gs == Victory  = pictures (pics ++ [png "res/victory-screen.png"])
              | otherwise = pictures pics
              where pics = [ render $ player gs
                             , pictures (map render (enemies gs) )
                             , pictures (map render (bombs gs) )
                             , pictures (map render (explosions gs)) ]

data CurrentState = Loading | Running | Paused | GameOver | Victory
        deriving(Show, Eq)

initGame :: GameState
initGame = GameState initPlayer level1 Loading (mkStdGen 0) Up initEnemies [] []

getRNumber :: IO Int
getRNumber = getStdRandom (randomR(1,100))

{-
    Generates random blocks by RNG in Gamestate
    Chance is now set at 60%
-}
setBreakableBlocks :: GameState -> IO GameState
setBreakableBlocks gs =
        do
            g <- return $ grid gs
            newGrid <- mapM setBreakableBlock g
            return gs { grid = newGrid, currentState = Running}

setBreakableBlock :: Field -> IO Field
setBreakableBlock f = do
                        rng <- getRNumber
                        let obj = gameObject f
                        return (case obj of
                            Empty | rng > 70 && not (isSafeZone f)      -> f { gameObject = StoneBlock}
                                  | otherwise                      -> f
                            _                                      -> f )

isSafeZone :: Field -> Bool
isSafeZone f | pos == (-375, 375) || pos == (-325, 375) || pos == (-375, 325) = True
             | otherwise = False
            where pos = fieldPosition f

-- Given an object that has an area, and a movable object, return true if there's a collision between these two, else false
checkCollision :: (HasArea a, Movable b) => b -> a -> Bool
checkCollision pl a = let (x,y) = getPos pl
                          (w,h) = (width pl, height pl) in
                      case getDir pl of
                            North   | inArea a (x,y+1)
                                        || inArea a (x+w,y+1) -> True
                                    | otherwise                         -> False
                            East    | inArea a (x+50,y)
                                        || inArea a (x+50,y-h) -> True
                                    | otherwise                         -> False
                            South   | inArea a (x,y-50)
                                        || inArea a (x+w,y-50) -> True
                                    | otherwise                         -> False
                            West    | inArea a (x-1,y)
                                        || inArea a (x-1,y-h) -> True
                                    | otherwise                         -> False

-- Modify the state of a bomb inside the gamestate using a function
modBombs:: GameState -> (Bombs -> Bombs) -> GameState
modBombs gstate f = gstate { bombs = f $ bombs gstate}

-- Modify the state of a grid inside the gamestate using a function
modGrid :: GameState -> (Grid -> Grid) -> GameState
modGrid gstate f = gstate { grid = f $ grid gstate}

-- Modify the state of a player using a function
modPlayer :: GameState -> (Player -> Player) -> GameState
modPlayer gstate f = gstate { player = f $ player gstate}

modEnemies :: GameState -> (Player -> Player) -> GameState
modEnemies gstate f = gstate {enemies = map f (enemies gstate) }

modifyDynamics :: GameState -> GameState
modifyDynamics gs | length explosionsTot > 0        = modifyPlayer modifyBombs
                  | otherwise                       = modifyBombs
                where modifyPlayer = \g -> modPlayer g $ checkCollisionBombs explosionsTot
                      modifyBombs = gs {grid = newGrid, bombs = newBombs, explosions = explosionsTot}
                      (newBombs, newExplosions) = setTimerBombs (bombs gs)
                      (newOldExplosions, newGrid) = modOldExplosions (grid gs) (explosions gs)
                      explosionsTot = newExplosions ++ newOldExplosions


setTimerBombs :: Bombs -> (Bombs,Explosions)
setTimerBombs bombs = let      setTimers = map explosionCountDown bombs
                               newBombs = filter (\b -> timeTillExplosion b > 0) setTimers
                               explodingBombs = filter (\b -> timeTillExplosion b == 0) setTimers
                               newExplosions = makeExplosions explodingBombs in
                      (newBombs, newExplosions)



modOldExplosions :: Grid -> Explosions -> (Explosions,Grid)
modOldExplosions gr explosions = let newGrid = foldl checkDestruction gr newExplosions
                                     newExplosions = setTimerExplosion $ map (checkCollisionEx gr) explosions in
                              (newExplosions, newGrid)



checkCollisionEx :: Grid -> Explosion -> Explosion
checkCollisionEx [] ex     = moveExplosion ex
checkCollisionEx (x:xs) ex | checkCollision ex x && explosionStatus ex == Destructed     = ex
                           | checkCollision ex x  = setExplosionDestructed $ moveExplosion ex
                           | otherwise                 = checkCollisionEx xs ex



--COLLISION DETECTION --
--BOMBS VS GRID--


--   checkDestructionBlocks :: Explosions -> Grid -> Grid
--   checkDestructionBlocks ex grid = foldl checkDestruction grid ex

checkDestruction :: Grid -> Explosion -> Grid
checkDestruction [] b = []
checkDestruction (x:xs) b | gameObject x == StoneBlock && inArea b (getPos x) = checkDestruction xs b
                          | otherwise                                         = x : checkDestruction xs b




--BOMBS VS PLAYER--
checkCollisionBombs :: Explosions -> a -> a
checkCollisionBombs [] p     = p
checkCollisionBombs (x:xs) p | checkCollision p x            = checkCollisionBombs xs $ setPlayerDead p
                             | otherwise                     = checkCollisionBombs xs p

setPlayerDead :: Player -> Player
setPlayerDead pl = pl { state = Dying}


--ENEMIES VS PLAYER--
checkCollisionEnemies :: [Player] -> Player -> Player
checkCollisionEnemies [] p = p
checkCollisionEnemies (x:xs) p | checkCollision p x     = checkCollisionEnemies xs $ setPlayerDead p
                              | otherwise               = checkCollisionEnemies xs p

--PLAYERS VS GRID--
--change the direction in which the player is positioned and possibly move player in that direction
changePlayerDir :: GameState -> Direction -> Player -> Player
changePlayerDir gstate dir player' = checkifMovePlayer gstate $ setDir dir player'

checkifMovePlayer :: GameState -> Player -> Player
checkifMovePlayer gs p  | checkCollisionField p $ grid gs     = newP
                        | otherwise                           = movePlayerInDir newP
        where newP = checkCollisionBombs (explosions gs) $ checkCollisionPlayer (enemies gs) p


checkCollisionField :: Player -> Grid -> Bool
checkCollisionField _ []     = False
checkCollisionField p (x:xs)  | gameObject x /= Empty &&  gameObject x /= PowerUp && checkCollision p x  = True
                              | otherwise                 = checkCollisionField p xs



