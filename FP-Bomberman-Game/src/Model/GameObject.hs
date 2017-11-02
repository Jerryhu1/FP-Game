module Model.GameObject where

 import Model.Typeclasses.Positioned
 import Model.Typeclasses.Destructible

 data GameObject = PowerUp 
                    | MetalBlock 
                    | StoneBlock 
                    | Empty
                    | Bomb
                    | Explosion
                    deriving(Show, Ord, Eq)

 data Status = UnExploded | Exploding
    deriving(Show, Ord, Eq)
 
 data ExplosionTime = Float
    deriving(Show, Ord, Eq)
 
 data ExplosionRadius = Int
    deriving(Show, Ord, Eq)
 
