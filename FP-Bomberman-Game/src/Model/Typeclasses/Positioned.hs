module Model.Typeclasses.Positioned where

    class Positioned a where
            pos :: a -> (Int, Int)

