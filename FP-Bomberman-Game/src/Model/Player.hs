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

data PlayerState = Walking | Idle | Hit deriving (Eq, Show)
data Health = Dead | Alive deriving (Eq, Show)

instance Positioned Player where
    getPos p = playerPosition p


instance Movable Player where
    setPos pos player = player { playerPosition = pos }
    setDir dir player = player { playerDirection = dir}

instance Show Player where
    show p = show(getPos p) ++ "Player: " ++ name p
                            ++ " Health: " ++ show(health p)
                            ++ " Direction: " ++ show (playerDirection p)
                            ++ " State: " ++ show (state p)


instance Renderizable Player where
    render p | health p == Alive && state p == Idle   = translate' (getPos p) $ playerIdlePictures p
             | health p == Alive && state p  == Walking = translate' (getPos p) $ sprite p
             | otherwise    = translate' (getPos p) $ color blue $ text "RIP"

initPlayer :: Player
initPlayer = Player "Jerry" Alive (-375,375) 10 West (0,0) Idle (png "res/bomberman-idle.png")

playerWidth :: Int
playerWidth = 29

playerHeight :: Int
playerHeight = 29

initEnemies :: [Player]
initEnemies = [Player "Monstertje" Alive (225,125) 5 South (225, 75) Idle (png "res/bomberman-idle.png"),
               Player "Monstertje2" Alive (325,-225) 5 East (325, -175) Idle (png "res/bomberman-idle.png")]

--if no collision occures, move player in the direction he is facing
movePlayerInDir :: Player -> Player
movePlayerInDir player' = case playerDirection player' of
                                West -> setPos (calcNewPos (-1,0) player') player'
                                East -> setPos (calcNewPos (1,0) player') player'
                                North -> setPos (calcNewPos (0,1) player') player'
                                South -> setPos (calcNewPos (0,-1) player') player'
--move player given a new 
calcNewPos :: Pos -> Player -> Pos
calcNewPos pos player' = getBound posTimesVel $ getPos player'
    where posTimesVel = (*.) pos (* (velocity player'))


getBound :: Pos -> Pos -> Pos
getBound (x,y) (x',y') = (newX, newY)
    where newX = max (-375) $ min 375 $ x+x'
          newY = max (-225) $ min 375 $ y+y'
    
getGridPos:: Player -> Pos
getGridPos p = (*.) midPosPlayer f
    where   midPosPlayer = (+.) (25,25) $ getPos p
            f = \x -> x - ((x-25) `mod` fieldSize)

playerWalkingPictures :: Player -> [Picture]
playerWalkingPictures p | playerDirection p == North  = [png "res/bomberman-walk-up-1.png", png "res/bomberman-walk-up-2.png"]
                | playerDirection p == South  = [png "res/bomberman-walk-down-1.png", png "res/bomberman-walk-down-2.png"]
                | playerDirection p == West  =  [png "res/bomberman-walk-left-1.png", png "res/bomberman-walk-left-2.png"]
                | otherwise                   = [png "res/bomberman-walk-right-1.png", png "res/bomberman-walk-right-2.png"]

playerIdlePictures :: Player -> Picture
playerIdlePictures p
                | playerDirection p == North  = png "res/bomberman-idle-up.png"
                | playerDirection p == South  = png "res/bomberman-idle.png"
                | playerDirection p == West  =  png "res/bomberman-idle-left-1.png"
                | otherwise                   = png "res/bomberman-idle-right-1.png"

-- Checks if the current picture is equal to any inside the list, and takes a different one if so
animatePlayer :: Player -> Player
animatePlayer p  | isJust $ currentPic = if fromJust (currentPic) == 1
                                         then p { sprite = (playerWalkingPictures p !! 0) }
                                         else  p { sprite = (playerWalkingPictures p !! 1) }
                 | otherwise = p {sprite = head $ playerWalkingPictures p }
                   where dir = playerDirection p
                         currentPic = elemIndex (sprite p) (playerWalkingPictures p)