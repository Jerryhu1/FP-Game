module Model.Bombs where

 import Graphics.Gloss.Game

 import Model.Typeclasses.Positioned
 import Model.Typeclasses.Renderizable

 ---BOMBS---
 data Bomb = Bomb {
                    bombPosition :: Pos,
                    timeTillExplosion :: Float,
                    spriteBomb :: Picture
                }

 type Bombs = [Bomb]

 instance Positioned Bomb where
    getPos = bombPosition

 instance HasArea Bomb where
    width b = 45
    height b = 45
    inArea b (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
        where   (x1,y1) = getPos b
                (x2,y2) = (x1+width b,y1-height b)

 instance Renderizable Bomb where
    render b = translate' (getPos b) (spriteBomb b)                    

 --adds a new bomb
 addBombs :: Pos -> Bombs -> Bombs
 addBombs pos bs = Bomb {bombPosition = pos, timeTillExplosion = 24, spriteBomb = png "res/bomb-1.png"} : bs

 --countdown till a bomb turns into an explosion
 explosionCountDown :: Bomb -> Bomb
 explosionCountDown bomb = bomb {timeTillExplosion = timeTillExplode}
    where timeTillExplode = timeTillExplosion bomb - 1

 --if the timer on a bomb runs out, turn the bomb into an explosion
 --controlled from function setTimerBombs in GameState.hs
 makeExplosions :: Bombs -> Explosions
 makeExplosions = concatMap (addExplosion . getPos)

 ---EXPLOSIONS--

 data Explosion = Explosion {
    explosionPosition :: Pos,
    explosionTime :: Float,
    explosionStatus :: ExplosionStatus,
    explosionDirection :: Direction,
    spriteExplosion :: Picture
 }

 data ExplosionStatus = Moving | Destructed | Mid
    deriving(Show, Eq) 

 type Explosions = [Explosion]

 instance Positioned Explosion where
    getPos = explosionPosition

 instance HasArea Explosion where
    width f = 49
    height f = 49
    inArea f (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
        where   (x1,y1) = getPos f
                (x2,y2) = (x1+ width f,y1-height f)

 instance Renderizable Explosion where
    render b = translate' (getPos b) (spriteExplosion b)                    
    
 instance Movable Explosion where
    setPos pos ex = ex {explosionPosition = pos}
    setDir dir ex = ex {explosionDirection = dir}
    getDir  = explosionDirection

 --if an explosion collides with a stoneblock, change the explosion status
 --this will make sure explosion can't destroy any more blocks
 setExplosionDestructed :: Explosion -> Explosion
 setExplosionDestructed ex = ex {explosionStatus = Destructed }

 --adds explosion on a position once the timer on a bomb has run out
 --5 explosions are dropped: one for each direction the explosion moves and one for a non-moving explosion centre
 addExplosion :: Pos -> Explosions
 addExplosion pos = [newEx pos dir | dir <- [North, East, South, West]] ++
                            [ Explosion { explosionPosition = pos, explosionTime = 9, explosionDirection = North, explosionStatus = Mid, spriteExplosion = png "res/explosion-center-5.png"}                            ]

 newEx :: Pos -> Direction -> Explosion
 newEx pos North = Explosion { explosionPosition = pos, explosionTime = 9, explosionDirection = North, explosionStatus = Moving, spriteExplosion = png "res/explosion-end-up-5.png"}
 newEx pos East = Explosion { explosionPosition = pos, explosionTime = 9, explosionDirection = East, explosionStatus = Moving, spriteExplosion = png "res/explosion-end-right.png"}
 newEx pos South = Explosion { explosionPosition = pos, explosionTime = 9, explosionDirection = South, explosionStatus = Moving, spriteExplosion = png "res/explosion-end-down-5.png"}
 newEx pos West = Explosion { explosionPosition = pos, explosionTime = 9, explosionDirection = West, explosionStatus = Moving, spriteExplosion = png "res/explosion-end-left-5.png"}
 
 --if there is no collision with a metal block, the explosion moves in a certain direction
 moveExplosion:: Explosion -> Explosion
 moveExplosion ex = let (x,y) = getPos ex in
                    case getDir ex of 
                        North -> setPos (x,y+5) ex
                        East -> setPos (5+x,y) ex
                        South -> setPos (x,y-5) ex
                        West -> setPos (x-5,y) ex

 --countdown till an explosion runs out
 setTimerExplosion :: Explosions -> Explosions
 setTimerExplosion ex = filter (\ex -> explosionTime ex > 0) $ map explosionCountDown' ex
 
 explosionCountDown' :: Explosion -> Explosion
 explosionCountDown' ex = ex {explosionTime = timeTillExplode}
     where timeTillExplode = explosionTime ex - 1



