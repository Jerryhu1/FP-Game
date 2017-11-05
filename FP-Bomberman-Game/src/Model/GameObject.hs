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




 makeExplosions :: Bombs -> Explosions
 makeExplosions bs = concat $ map (\x -> addExplosion $ getPos x) bs



 
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
    width b = 49
    height b = 49
    inArea b (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
        where   (x1,y1) = getPos b
                (x2,y2) = (x1+width b,y1-height b)

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
    explosionStatus :: ExplosionStatus,
    explosionDirection :: Direction,
    spriteExplosion :: Picture
 }

 data ExplosionStatus = Moving | Destructed

 type Explosions = [Explosion]

 instance Positioned Explosion where
    getPos b = explosionPosition b

 instance HasArea Explosion where
    width b = 49
    height b = 49
    inArea b (x,y) = (x1-r <= x && x <= x2+r && y2 <= y && y <= y1) || (x1 <= x && x <= x2 && y2-r <= y && y <= y1+r)
            where   (x1,y1) = getPos b
                    (x2,y2) = (x1+width b, y1-height b)
                    r = 50

 instance Renderizable Explosion where
    render b = translate' (getPos b) $ color (dark red) $ rectangleSolid 50 50

 instance Movable Explosion where
    setPos pos ex = ex {explosionPosition = pos}
    setDir dir ex = ex {explosionDirection = dir}
    getDir ex = explosionDirection ex

 addExplosion :: Pos -> Explosions
 addExplosion pos = [newEx pos dir | dir <- [North, East, South, West]]

 newEx :: Pos -> Direction -> Explosion
 newEx pos dir = Explosion { explosionPosition = pos, explosionTime = 12, explosionDirection = dir, explosionStatus = Moving, spriteExplosion = png "res/bomb-1.png"}

 moveExplosions:: Explosions -> Explosions
 moveExplosions = map moveExplosion

 moveExplosion:: Explosion -> Explosion
 moveExplosion ex = let (x,y) = getPos ex in
                    case getDir ex of 
                        North -> setPos (x,y+5) ex
                        East -> setPos (5+x,y) ex
                        South -> setPos (x,y-5) ex
                        West -> setPos (x-5,y) ex

 setTimerExplosion :: Explosions -> Explosions
 setTimerExplosion ex = filter (\ex -> explosionTime ex > 0) $ map explosionCountDown' ex
 
 explosionCountDown' :: Explosion -> Explosion
 explosionCountDown' ex = ex {explosionTime = timeTillExplode}
     where timeTillExplode = (explosionTime ex)-1



