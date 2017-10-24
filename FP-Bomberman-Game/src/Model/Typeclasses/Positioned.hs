module Model.Typeclasses.Positioned where

type Pos = (Int , Int)
type Vel = (Int , Int)

class Positioned a where
    getPos :: a -> Pos
    getX :: a -> Int
    getY :: a -> Int


class Movable a where
    setPos :: Pos -> a -> a


