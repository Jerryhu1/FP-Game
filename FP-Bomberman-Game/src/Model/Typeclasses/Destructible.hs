module Model.Typeclasses.Destructible where

    class Destructible a where
        getDestroyed :: a -> b

