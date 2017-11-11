module Model.PowerUp where

import Graphics.Gloss

import Model.Typeclasses.Positioned
import Model.Typeclasses.Renderizable
import Model.Typeclasses.Effectable

import Model.Player

data SpeedBoost = SpeedBoost {
                     speedBoostPosition :: Pos,
                     speedBoostAmount   :: Vel
                  }deriving(Show, Eq)

instance Renderizable SpeedBoost where
        render s = translate' (speedBoostPosition s) (color blue $ rectangleSolid 50.0 50.0)

instance Positioned SpeedBoost where
        getPos b = speedBoostPosition b

instance HasArea SpeedBoost where 
        width  b = 49
        height b = 49
        inArea b (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
                where   (x1,y1) = getPos b
                        (x2,y2) = (x1+width b,y1-height b)

instance Effectable SpeedBoost where
        applyEffectOnPlayer s pl = pl { velocity = (velocity pl + (speedBoostAmount s))}
    

data ExtraBomb = ExtraBomb {
    extraBombPosition :: Pos
}deriving(Show,Eq)

instance Renderizable ExtraBomb where
       render e = translate' (extraBombPosition e) (color blue $ rectangleSolid 50.0 50.0)

instance Positioned ExtraBomb where
       getPos b = extraBombPosition b

instance HasArea ExtraBomb where 
       width  b = 49
       height b = 49
       inArea b (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
        where   (x1,y1) = getPos b
                (x2,y2) = (x1+width b,y1-height b)

instance Effectable ExtraBomb where
        applyEffectOnPlayer = undefined




