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

data PowerUpType = SpeedBoost | ExtraBomb

instance Positioned PowerUp where
        getPos p = powerUpPosition p

instance HasArea PowerUp where
        width  b = 49
        height b = 49
        inArea b (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
            where (x1,y1) = getPos b
                  (x2,y2) = (x1+width b,y1-height b)

instance Renderizable PowerUp where
        render (PowerUp x SpeedBoost)   = translate' x $ png "res/powerup-speed-boost.png"
        render (PowerUp x ExtraBomb)    = translate' x $ png "res/powerup-extra-bomb.png"


applyEffectOnPlayer :: PowerUp -> Player -> Player
applyEffectOnPlayer (PowerUp _ SpeedBoost) pl  = pl { velocity = 5 + velocity pl}
applyEffectOnPlayer (PowerUp _ ExtraBomb) pl   = pl


addNewPowerUp :: Pos -> StdGen -> PowerUp
addNewPowerUp pos r | rng == 0  = PowerUp pos SpeedBoost
                    | otherwise = PowerUp pos ExtraBomb
                where rng = fst $ randomR (0,1) r :: Int




