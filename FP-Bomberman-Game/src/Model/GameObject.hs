module Model.GameObject where

 import Model.Typeclasses.Positioned
 import Model.Typeclasses.Destructible
 import Model.Typeclasses.Renderizable
 
 data Block  = Block {}
 blockSize :: Float
 blockSize = 50.0

 data GameObject = PowerUp 
        | MetalBlock 
        | StoneBlock 
        | Explosion 
        | Empty
        deriving(Show, Ord, Eq)
    -- Misschien PowerUp onderdeel maken van Metalblock?

 data Bomb = Bomb {
        explosionRadius :: (Int, Int),
        elapsedTime :: Float,
        bombPosition :: (Int,Int)
        }
 instance Renderizable Bomb where
        render a = color red $ circleSolid 15
      

     
 instance Positioned Bomb where
        getPos b = bombPosition b

 setTimer :: Bomb -> Bomb 
 setTimer bomb = bomb {elapsedTime = 1 + elapsedTime bomb}


 --alle plaatjes
 instance Renderizable PowerUp where
        render a = color yellow $ rectangleSolid blockSize blockSize
 
 grassColor :: Color
 grassColor = green
 
 instance Renderizable Empty where
        rander a = color grassColor $ rectangleSolid blockSize blockSize              
        
 stoneColor :: Color
 stoneColor = dark orange       
 
 instance Renderizable Stone where
        render a = color stoneColor $ rectangleSolid blockSize blockSize

 instance Renderizable Metalblock where
        render a = color (greyN 0.5) $ rectangleSolid blockSize blockSize

 instance Renderizable Explosion where
        render a = color (dark red) $ rectangleSolid blockSize blockSize
        
