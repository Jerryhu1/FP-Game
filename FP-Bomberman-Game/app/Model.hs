module Model where
    
    class Movable a where
        move :: a -> (Int, Int) -> a
        x :: Int
        y :: Int
    
    class Living a where
        die :: a -> a
        health :: Int
        name :: String
    
    class Destructible a where
        getDestroyed :: a -> b
    
    data GameState = GameState {
       player   :: Player,
       grid     :: Grid,
       enemies  :: [Enemy]
    }
    
    data Enemy = Enemy {}
    
    data Player = Player {}
    
    instance Movable Player where
        x n = n
        y m = m
        move Player (n,m) = Player
    
    instance Show Player where
       show p = "Player: " ++ name p ++ " Health: " ++ show(health p)
    
    data Field = Field {
       gameObject :: GameObject
    }
    
    type Grid = [Field]
    
    data GameObject = Powerup | MetalBlock | StoneBlock | Empty
    
    initialState :: GameState
    initialState = GameState ShowNothing 0 (Player 100 "Jerry")