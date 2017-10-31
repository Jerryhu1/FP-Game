module Model.Player where    
    
import Model.Typeclasses.Positioned
import Model.Grid

data Player = Player {
        name :: String,
        health :: Int,
        playerPosition :: Pos,
        velocity :: Vel,
        --moveSpeed :: Double,
        sprite :: String
        }

instance Positioned Player where
    getPos p = playerPosition p
    getX p = fst $ getPos p
    getY p = snd $ getPos p

instance Movable Player where
    setPos pos player = player { playerPosition = pos }

instance Show Player where
    show p = show(getPos p) ++ "Player: " ++ name p ++ " Health: " ++ show(health p)



    
initPlayer :: Player
initPlayer = Player "Jerry" 100 (-375,375) 10 "test"

getGridPos:: Player -> Pos
getGridPos p = (*.) midPosPlayer f
    where   midPosPlayer = (+.) (25,25) $ getPos p
            f = \x -> x - ((x-25) `mod` fieldSize)

     
