module Model.GameObject where

 import Graphics.Gloss.Game

 import Model.Typeclasses.Positioned
 import Model.Typeclasses.Renderizable
 import Model.Typeclasses.Destructible

 data GameObject = PowerUp
                    | MetalBlock
                    | StoneBlock
                    | Empty
                    deriving(Show, Ord, Eq)

 instance Renderizable GameObject where
        render MetalBlock = png "res/metal-block.png"
        render StoneBlock = png "res/stone-block.png"
        render Empty      = png "res/grass.png"
        render PowerUp    = undefined


 setTimerBombs :: Bombs -> Explosions -> (Bombs,Explosions)
 setTimerBombs bombs ex = let   setTimers = map explosionCountDown bombs
                                newBombs = filter (\b -> timeTillExplosion b > 0) setTimers
                                explodingBombs = filter (\b -> timeTillExplosion b == 0) setTimers
                                newExplosions = makeExplosions explodingBombs ++ setTimerExplosion ex in
                          (newBombs, newExplosions)

 makeExplosions :: Bombs -> Explosions
 makeExplosions bs = map (\x -> addExplosion $ getPos x) bs



 
 ---BOMBS---
 data Bomb = Bomb {
                    bombPosition :: Pos,
                    timeTillExplosion :: Float,
                    spriteBomb :: Picture
                }

 type Bombs = [Bomb]

 instance Positioned Bomb where
    getPos b = bombPosition b

 instance HasArea Bomb where
    inArea b (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
        where   (x1,y1) = getPos b
                (x2,y2) = (+.) (x1,y1) (49, -49)

 instance Renderizable Bomb where
    render b = translate' (getPos b) (spriteBomb b)                    


 addBombs :: Pos -> Bombs -> Bombs
 addBombs pos bs = Bomb {bombPosition = pos, timeTillExplosion = 24, spriteBomb = png "res/bomb-1.png"} : bs


 explosionCountDown :: Bomb -> Bomb
 explosionCountDown bomb = bomb {timeTillExplosion = timeTillExplode}
    where timeTillExplode = (timeTillExplosion bomb)-1


 ---EXPLOSIONS--
 data Explosion = Explosion {
    explosionPosition :: Pos,
    explosionTime :: Float,
    explosionRadius :: Int,
    spriteExplosion :: Picture
 }


 type Explosions = [Explosion]

 instance Positioned Explosion where
    getPos b = explosionPosition b

 instance HasArea Explosion where
    inArea b (x,y) = (x1-r <= x && x <= x2+r && y2 <= y && y <= y1) || (x1 <= x && x <= x2 && y2-r <= y && y <= y1+r)
            where   (x1,y1) = getPos b
                    (x2,y2) = (+.) (x1,y1) (49, -49)
                    r = 50 * explosionRadius b

 instance Renderizable Explosion where
    render b = let r = fromIntegral $ explosionRadius b in
            translate' (getPos b) $ color (dark red) $ rectangleSolid (50*r) (50*r)

 addExplosion :: Pos -> Explosion
 addExplosion pos = Explosion { explosionPosition = pos, explosionTime = 24, explosionRadius = 2, spriteExplosion = png "res/bomb-1.png"}

 setTimerExplosion :: Explosions -> Explosions
 setTimerExplosion ex = filter (\ex -> explosionTime ex > 0) $ map explosionCountDown' ex
 
 explosionCountDown' :: Explosion -> Explosion
 explosionCountDown' ex = ex {explosionTime = timeTillExplode}
     where timeTillExplode = (explosionTime ex)-1



