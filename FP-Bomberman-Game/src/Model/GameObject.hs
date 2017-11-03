module Model.GameObject where

 import Model.Typeclasses.Positioned
 import Model.Typeclasses.Destructible
 
 data GameObject = PowerUp 
                    | MetalBlock 
                    | StoneBlock 
                    | Empty
                    deriving(Show, Ord, Eq)


 ---BOMBS---
 data Bomb = Bomb {bombPosition :: Pos, bombStatus :: BombStatus, explosionTime :: Float, explosionRadius :: Int}
 
 data BombStatus = UnExploded | Exploding
    deriving(Show, Ord, Eq)
 
 type Bombs = [Bomb]
    
 instance Positioned Bomb where
    getPos b = bombPosition b

 instance HasArea Bomb where
    inArea b (x,y)      | bombStatus b == UnExploded = x1 <= x && x <= x2 && y2 <= y && y <= y1
                        | otherwise                 = (x1-r <= x && x <= x+r && y2 <= y && y <= y1) ||
                                                      (x1 <= x && x <= x && y2-r <= y && y <= y1+r)
        where   (x1,y1) = getPos b 
                (x2,y2) = (+.) (x1,y1) (49, -49)  
                r = 50 * explosionRadius b  

 addBomb :: Pos -> Bombs -> Bombs
 addBomb pos bs = Bomb {bombPosition = pos, bombStatus = UnExploded, explosionTime = 24, explosionRadius = 2} : bs
              
 setTimer :: Bombs -> Bombs
 setTimer = map explosionCountDown
    
 explosionCountDown :: Bomb -> Bomb
 explosionCountDown bomb = bomb {explosionTime = timeTillExplode}
    where timeTillExplode = (explosionTime bomb)-1

    
 ---EXPLOSIONS
 

 
 
