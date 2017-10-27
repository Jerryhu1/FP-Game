module Model.Player where    
    
import Model.Typeclasses.Positioned
import Model.Grid

data Player = Player {
        name :: String,
        health :: Int,
        playerPosition :: Pos,
        velocity :: Vel,
        direction :: Direction,
        --moveSpeed :: Double,
        sprite :: String
        }

data Direction = North | South | East | West deriving(Show)

instance Positioned Player where
    getPos p = playerPosition p
    getX p = fst $ getPos p
    getY p = snd $ getPos p

instance Movable Player where
    setPos pos player = player { playerPosition = pos }

instance Show Player where
    show p = show(getPos p) ++ "Player: " ++ name p ++ " Health: " ++ show(health p) ++ " Direction: " ++ show (direction p)

initPlayer :: Player
initPlayer = Player "Jerry" 100 (-375,375) 10 North "test"

getGridPos:: Player -> Pos
getGridPos p = (\(x,y) -> (f x, f y)) (getPos p)
    where f = (\x -> x-25) . (*fieldSize) . (`div` fieldSize)

     
