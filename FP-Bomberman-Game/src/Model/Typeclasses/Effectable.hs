module Model.Typeclasses.Effectable where

import Model.Player

class Effectable a where
    applyEffectOnPlayer ::  a -> Player -> Player



