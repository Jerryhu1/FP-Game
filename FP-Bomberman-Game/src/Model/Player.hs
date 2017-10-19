module Model.Player where

     import Model.Typeclasses.Positioned

     data Player = Player {
            name :: String,
            health :: Int,
            playerPosition :: (Int, Int),
            moveSpeed :: Double,
            direction :: Direction,
            sprite :: String
     }

     data Direction = Up | Down | Left | Right


     instance Positioned Player where
        pos p = playerPosition p

     instance Show Player where
        show p = "Player: " ++ name p ++ " Health: " ++ show(health p)
