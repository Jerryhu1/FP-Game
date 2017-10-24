module Model.Player where    
    
import Model.Typeclasses.Positioned

data Player = Player {
        name :: String,
        health :: Int,
        playerPosition :: Pos,
        velocity :: Vel,
        --moveSpeed :: Double,
        sprite :: String
        }

instance Positioned Player where
    getPos player = playerPosition player 
    getX player = fst $ playerPosition player 
    getY player = snd $ playerPosition player

instance Movable Player where
    setPos vel player = player { playerPosition = addVel (getPos player) vel }
        where addVel (x,y) (a,b) = (x+a,y+b)
    

instance Show Player where
    show p = show(getPos p) ++ "Player: " ++ name p ++ " Health: " ++ show(health p)

     
     