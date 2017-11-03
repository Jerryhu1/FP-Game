module Model.Player where    
    
import Model.Typeclasses.Positioned
import Model.Grid


data Player = Player {
        name :: String,
        health :: Int,
        playerPosition :: Pos,
        velocity :: Vel,
        playerDirection :: Direction,
        --moveSpeed :: Double,
        sprite :: String
        }

            

instance Positioned Player where
    getPos p = playerPosition p


instance Movable Player where
    setPos pos player = player { playerPosition = pos }
    setDir dir player = player { playerDirection = dir}

instance Show Player where
    show p = show(getPos p) ++ "Player: " ++ name p ++ " Health: " ++ show(health p) ++ " Direction: " ++ show (playerDirection p)

initPlayer :: Player
initPlayer = Player "Jerry" 100 (-375,375) 10 West "test"
    
    


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
          newY = max (-375) $ min 375 $ y+y'
    
getGridPos:: Player -> Pos
getGridPos p = (*.) midPosPlayer f
    where   midPosPlayer = (+.) (25,25) $ getPos p
            f = \x -> x - ((x-25) `mod` fieldSize)

     
