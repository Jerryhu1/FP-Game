module Model.PowerUp where

import Graphics.Gloss
import Graphics.Gloss.Game
import System.Random

import Model.Typeclasses.Positioned
import Model.Typeclasses.Renderizable

import Model.Player


data PowerUp = PowerUp {
                powerUpPosition :: Pos,
                powerUpType     :: PowerUpType
                }

data PowerUpType = SpeedBoost | ExtraBomb | FasterBomb

instance Positioned PowerUp where
        getPos = powerUpPosition

instance HasArea PowerUp where
        width  b = 49
        height b = 49
        inArea b (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
            where (x1,y1) = getPos b
                  (x2,y2) = (x1+width b,y1-height b)

instance Renderizable PowerUp where
        render (PowerUp x SpeedBoost)   = translate' x $ png "res/powerup-speed-boost.png"
        render (PowerUp x ExtraBomb)    = translate' x $ png "res/powerup-extra-bomb.png"
        render (PowerUp x FasterBomb)    = translate' x $ png "res/powerup-faster-bomb.png"
        

applyEffectOnPlayer :: PowerUp -> Player -> Player
applyEffectOnPlayer (PowerUp _ SpeedBoost) pl  = pl { velocity = max 15 (5 + velocity pl)}
applyEffectOnPlayer (PowerUp _ ExtraBomb) pl   = pl { timeTillNewBomb = 0 : (timeTillNewBomb pl)}
applyEffectOnPlayer (PowerUp _ FasterBomb) pl   = pl { explosionSpeed = max 12 $(explosionSpeed pl)-5}



addNewPowerUp :: Pos -> StdGen -> PowerUp
addNewPowerUp pos r | rng == 0  = PowerUp pos SpeedBoost
                    | rng == 1  = PowerUp pos FasterBomb
                    | otherwise = PowerUp pos ExtraBomb
                where rng = fst $ randomR (0,2) r :: Int




