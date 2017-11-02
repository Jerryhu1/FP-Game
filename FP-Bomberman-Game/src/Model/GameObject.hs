module Model.GameObject where

 import Model.Typeclasses.Positioned
 import Model.Typeclasses.Destructible
 import Model.Typeclasses.Renderizable
 



--BOMMEN--
 data Bomb = Bomb {
        explosionRadius :: (Int, Int),
        elapsedTime :: Float,
        bombPosition :: Pos
        stateExplosion :: StateExplosion
        }

 data StateExplosion = Pending | Explosion
        

 instance Positioned Bomb where
        getPos b = bombPosition b


 instance Renderizable Bomb where
        render a = color red $ circleSolid 15     

 instance HasArea Bomb where
        inArea f (x,y) = let (x1,y1) = (getX f, getY f )
                             (x2,y2) = (+.) (x1,y1) (49, -49)
                         in x1 <= x && x <= x2 && y2 <= y && y <= y1

 setTimer :: Bomb -> Bomb 
 setTimer bomb = bomb {elapsedTime = 1 + elapsedTime bomb}

--POWER UPS--
{- data PowerUp = PowerUp{
        
 }
 instance Renderizable PowerUp where
        render a = color yellow $ rectangleSolid blockSize blockSize
 -}

 blockSize :: Float
 blockSize = 50.0
    
 --BLOCKS--
 data StoneBlock = StoneBlock {
        stoneBlockPosition :: Pos
        stoneBlockStatus :: StoneBlockStatus
 }

 data StoneBlockStatus = Destructed | UnDestructed

 stoneColor :: Color
 stoneColor = dark orange  

 instance Positioned StoneBlock where
        getPos a = stoneBlockPosition a

 instance Renderizable StoneBlock where
        render a = color stoneColor $ rectangleSolid blockSize blockSize

 instance HasArea StoneBlock where
        inArea f (x,y) = let (x1,y1) = (getX f, getY f )
                             (x2,y2) = (+.) (x1,y1) (49, -49)
                         in x1 <= x && x <= x2 && y2 <= y && y <= y1

                         
 --METALBLOCKS--       
 data MetalBlocks = MetalBlock {
        metalBlockPosition :: Pos
 }
 instance Positioned MetalBlock where
        getPos a = metalBlockPosition a

 instance Renderizable Metalblock where
        render a = color (greyN 0.5) $ rectangleSolid blockSize blockSize

 instance HasArea MetalBlock where
        inArea f (x,y) = let (x1,y1) = (getX f, getY f )
                             (x2,y2) = (+.) (x1,y1) (49, -49)
                         in x1 <= x && x <= x2 && y2 <= y && y <= y1        
 {-
 instance Renderizable Explosion where
        render a = color (dark red) $ rectangleSolid blockSize blockSize
        -}
        
