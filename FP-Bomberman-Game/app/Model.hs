module Model where

    class HasPosition a where
        pos :: a -> (Int, Int)
    
    class Destructible a where
        getDestroyed :: a -> b
        
    data GameState = GameState {
        player   :: Player,
        grid     :: Grid
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

    data Field = Field {
        fieldPosition :: (Int,Int),
        gameObject :: GameObject
    }

    type Grid = [Field]

    data Block = Block {
        
    }
    
    data GameObject = Powerup | MetalBlock | StoneBlock | Empty

    instance HasPosition Field where 
        pos f = fieldPosition f

    instance HasPosition Player where
        pos p = playerPosition p

    instance HasPosition Bomb where
        pos b = bombPosition b
    
    instance Show Player where
        show p = "Player: " ++ name p ++ " Health: " ++ show(health p)
    
    initPlayer :: Player
    initPlayer = Player "Jerry" 100 (1,1) "test"

    initGame :: GameState
    initGame = GameState initPlayer $ createGrid 5 
    
    createGrid :: Int -> Grid
    createGrid n = [Field (x,y) Empty| x <- [0..n], y <- [0..n]]
    
    setNewPlayer :: String -> Player
    setNewPlayer name = Player name 100 (0,0) "player.png"

    