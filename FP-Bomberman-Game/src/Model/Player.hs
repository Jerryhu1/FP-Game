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
            sprite :: Picture
        }deriving(Eq)

data PlayerState = Walking | Idle | Dying deriving (Eq, Show)
data Health = Dead | Alive deriving (Eq, Show)

instance Positioned Player where
    getPos p = playerPosition p


instance Movable Player where
    setPos pos player = player { playerPosition = pos }
    setDir dir player = player { playerDirection = dir}
    getDir player = playerDirection player

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
                where newPos = (+.) (-5,15) $ getPos p

initPlayer :: Player
initPlayer = Player "Jerry" Alive (-370,370) 10 West (0,0) Idle (png "res/bomberman-idle.png")


initEnemies :: [Player]
initEnemies = [Player "Monstertje" Alive (375,370) 5 South (225, 75) Idle (png "res/enemy-idle-down-1.png")]

--if no collision occurs, move player in the direction he is facing
movePlayerInDir :: Player -> Player
movePlayerInDir player' = case playerDirection player' of
                                West -> setPos (calcNewPos (-1,0) player') player'
                                East -> setPos (calcNewPos (1,0) player') player'
                                North -> setPos (calcNewPos (0,1) player') player'
                                South -> setPos (calcNewPos (0,-1) player') player'
--move player given a new 
calcNewPos :: Pos -> Player -> Pos
calcNewPos pos player' = getBound posTimesVel $ getPos player'
                         where posTimesVel = (*.) pos (* (min 30 $ velocity player'))
                               

-- Get the boundaries of a given position
getBound :: Pos -> Pos -> Pos
getBound (x,y) (x',y') = (newX, newY)
    where newX = max (-375) $ min 375 $ x+x'
          newY = max (-225) $ min 375 $ y+y'

getGridPos:: Player -> Pos
getGridPos p = (*.) midPosPlayer f
    where   midPosPlayer = (+.) (25,25) $ getPos p
            f = \x -> x - ((x-25) `mod` fieldSize)

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

animateWalkingPlayer :: Player -> Player
animateWalkingPlayer p | isJust $ currentPic =
                                if fromJust (currentPic) == 1
                                then p { sprite = (playerWalkingPictures p !! 0) }
                                else  p { sprite = (playerWalkingPictures p !! 1) }
                       | otherwise = p {sprite = head $ playerWalkingPictures p }
                            where dir = playerDirection p
                                  currentPic = elemIndex (sprite p) (playerWalkingPictures p)

animateDyingPlayer :: Player -> Player
animateDyingPlayer p | isNothing currentPicIndex                   = p { sprite = head (playerDyingPictures p)}
                     | fromJust currentPicIndex == frameAmount - 1 = p { sprite = blank, health = Dead }
                     | otherwise                                   = p {  sprite = nextPicture }
                    where frameAmount      = length $ playerDyingPictures p
                          currentPicIndex  = elemIndex (sprite p) (playerDyingPictures p)
                          nextPicture      = (playerDyingPictures p) !! (fromJust currentPicIndex + 1)