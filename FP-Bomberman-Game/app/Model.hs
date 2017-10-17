module Model where

    class HasPosition a where
        pos :: a -> (Int, Int)
    
    class Destructible a where
        getDestroyed :: a -> b
        
    data GameState = GameState {
       player   :: Player,
       grid     :: Grid
  --     enemies  :: [Enemy]
    }
    
    data Player = Player {
        name :: String,
        health :: Int,
        playerPosition :: (Int, Int),
        sprite :: String
    }

    data Bomb = Bomb {
        explosionRadius :: (Int, Int),
        explodeTime :: Int,
        bombPosition :: (Int,Int)
    }

    
    instance HasPosition Player where
        pos p = playerPosition p

    instance HasPosition Bomb where
        pos b = bombPosition b
    
    instance Show Player where
       show p = "Player: " ++ name p ++ " Health: " ++ show(health p)
    
    data Field = Field {
        fieldPosition :: (Int,Int),
        gameObject :: GameObject
    }

    instance HasPosition Field where 
        pos f = fieldPosition f
    
    type Grid = [Field]
    
    data GameObject = Powerup | MetalBlock | StoneBlock | Empty