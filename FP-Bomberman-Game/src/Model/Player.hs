module Model.Player where    

import Graphics.Gloss.Game
import Data.Maybe
import Data.List

import Model.Typeclasses.Positioned
import Model.Typeclasses.Renderizable

import Model.Grid


data Player = Player {
            name :: String,
            health :: Health,
            playerPosition :: Pos,
            velocity :: Vel,
            playerDirection :: Direction,
            goal :: Pos,
            state :: PlayerState,
            sprite :: Picture,
            timeTillExplosionPlayer :: Int,
            timeTillNewBomb :: [Int]
        }deriving(Eq)

data PlayerState = Walking | Idle | Dying deriving (Eq, Show)
data Health = Dead | Alive deriving (Eq, Show)

instance Positioned Player where
    getPos = playerPosition


instance Movable Player where
    setPos pos player = player { playerPosition = pos }
    setDir dir player = player { playerDirection = dir}
    getDir            = playerDirection

instance Show Player where
    show p = show(getPos p) ++ "Player: " ++ name p
                            ++ " Health: " ++ show(health p)
                            ++ " Direction: " ++ show (playerDirection p)
                            ++ " State: " ++ show (state p)

instance HasArea Player where
    width p = 29
    height p = 39
    inArea p (x,y) = let (x1,y1) = getPos p
                         (x2,y2) = (x1+width p, y1-height p)
                     in x1 <= x && x <= x2 && y2 <= y && y <= y1
                     
instance Renderizable Player where
    render p | health p == Alive && state p == Idle    = translate' newPos $ playerIdlePictures p
             | health p == Alive && state p == Walking = translate' newPos $ sprite p
             | health p == Alive && state p == Dying   = translate' (getPos p) $ sprite p
             | otherwise                               = blank
                where   (x,y) = getPos p
                        newPos = (x-5,y+15)

initPlayer :: Player
initPlayer = Player "Jerry" Alive (-370,370) 5 West (0,0) Idle (png "res/bomberman-idle.png") 24 [0]


initEnemies :: [Player]
initEnemies = [Player "Monstertje1" Alive (375,370) 5 South (225, 75) Walking (png "res/enemy-idle-down-1.png") 24 [0],
               Player "Monstertje2" Alive (275, -375) 5 South (225, -375) Walking (png "res/enemy-idle-down-1.png") 24 [0] ]

setPlayerState :: PlayerState -> Player -> Player
setPlayerState pState pl = pl { state = pState }


--PLAYER MOVEMENT--

--if no collision occurs, move player in the direction he is facing
movePlayerInDir :: Player -> Player
movePlayerInDir player' = case playerDirection player' of
                                West -> setPos (calcNewPos (-1,0) player') player'
                                East -> setPos (calcNewPos (1,0) player') player'
                                North -> setPos (calcNewPos (0,1) player') player'
                                South -> setPos (calcNewPos (0,-1) player') player'

--move player given a new position
calcNewPos :: Pos -> Player -> Pos
calcNewPos (x,y) player' = getBound posTimesVel $ getPos player'
                         where  f = \x -> x * (min 30 $ velocity player')
                                posTimesVel = (f x, f y )
                               

-- Make sure player can't be positioned outside the grid
getBound :: Pos -> Pos -> Pos
getBound (x,y) (x',y') = (newX, newY)
    where newX = max (-375) $ min 375 $ x+x'
          newY = max (-225) $ min 375 $ y+y'

--get the grid position of Player 
--used for dropping bombs
getGridPos:: Player -> Pos
getGridPos p =  (f (x+25), f (y+25))
    where   (x,y) = getPos p
            f = \x -> x - ((x-25) `mod` fieldSize)

--DROPPING BOMBS--

--countdown till a new bomb can be dropped
--updated every step
timerCountDownPlayer :: Player -> Player
timerCountDownPlayer pl = pl {timeTillNewBomb = map (\x -> max 0 (x-1)) $timeTillNewBomb pl }

--tests if a player is able to drop a bomb
--this is the case when one of the timers is equal to 0
canDropBomb :: Player -> Bool
canDropBomb pl  | 0 `elem` timeTillNewBomb pl   = True
                | otherwise                     = False

--if a bomb can be dropped, set one of the available timers to 24
--the FasterBomb powerup lowers the time till a bomb explodes
setTimerPlayer :: Player -> Player
setTimerPlayer pl = pl {timeTillNewBomb = replaceTimer n $timeTillNewBomb pl}
    where   n = timeTillExplosionPlayer pl

replaceTimer :: Int -> [Int] -> [Int]
replaceTimer _ [] = []
replaceTimer n (x:xs) | x == 0    = n: xs
                      | otherwise = x : replaceTimer n xs
                    


--PLAYER VISUALIZATION--

-- Returns a list of pictures that represent a walking direction
playerWalkingPictures :: Player -> [Picture]
playerWalkingPictures p
                | playerDirection p == North  = [png "res/bomberman-walk-up-1.png", png "res/bomberman-walk-up-2.png"]
                | playerDirection p == South  = [png "res/bomberman-walk-down-1.png", png "res/bomberman-walk-down-2.png"]
                | playerDirection p == West  =  [png "res/bomberman-walk-left-1.png", png "res/bomberman-walk-left-2.png"]
                | otherwise                   = [png "res/bomberman-walk-right-1.png", png "res/bomberman-walk-right-2.png"]

-- Returns a Picture based on the direction of the player that is idle
playerIdlePictures :: Player -> Picture
playerIdlePictures p
                | playerDirection p == North  = png "res/bomberman-idle-up.png"
                | playerDirection p == South  = png "res/bomberman-idle.png"
                | playerDirection p == West  =  png "res/bomberman-idle-left-1.png"
                | otherwise                   = png "res/bomberman-idle-right-1.png"

playerDyingPictures :: Player -> [Picture]
playerDyingPictures p = [png "res/bomberman-dying-1.png", png "res/bomberman-dying-2.png",
                         png "res/bomberman-dying-3.png", png "res/bomberman-dying-4.png",
                         png "res/bomberman-dying-5.png", png "res/bomberman-dying-6.png",
                         png "res/bomberman-dying-7.png"]

-- Checks if the current picture is equal to any inside the list, and takes a different one if so
animatePlayer :: Player -> Player
animatePlayer p  | state p == Walking = animateWalkingPlayer p
                 | state p == Dying   = animateDyingPlayer p
                 | otherwise          = p

-- Animates a walking player, toggles between two different pictures when the player is walking
animateWalkingPlayer :: Player -> Player
animateWalkingPlayer p | isJust currentPic =
                                if fromJust currentPic == 1
                                then p { sprite = head $ playerWalkingPictures p  }
                                else  p { sprite = playerWalkingPictures p !! 1 }
                       | otherwise = p {sprite = head $ playerWalkingPictures p }
                            where dir = playerDirection p
                                  currentPic = elemIndex (sprite p) (playerWalkingPictures p)

-- Animates a dying player, keeps taking the next picture each step, and if it reaches the last picture, change the state to Dead
animateDyingPlayer :: Player -> Player
animateDyingPlayer p | isNothing currentPicIndex                   = p { sprite = head (playerDyingPictures p)}
                     | fromJust currentPicIndex == frameAmount - 1 = p { sprite = blank, health = Dead }
                     | otherwise                                   = p {  sprite = nextPicture }
                    where frameAmount      = length $ playerDyingPictures p
                          currentPicIndex  = elemIndex (sprite p) (playerDyingPictures p)
                          nextPicture      = playerDyingPictures p !! (fromJust currentPicIndex + 1)