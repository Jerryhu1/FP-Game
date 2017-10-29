module Model.Bomb where

import Typeclasses.Positioned
import Typeclasses.Destructible

     data Bomb = Bomb {
            explosionRadius :: (Int, Int),
            explodeTime :: Float,
            bombPosition :: (Int,Int)
        }

     instance Positioned Bomb where
            pos b = bombPosition b
