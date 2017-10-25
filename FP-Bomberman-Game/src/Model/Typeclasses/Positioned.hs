module Model.Typeclasses.Positioned where

type Pos = (Int , Int)
type Vel = Int

class Positioned a where
    getPos :: a -> Pos
    getX :: a -> Int
    getY :: a -> Int


class Movable a where
    setPos :: Pos -> a -> a

(+.) :: Pos -> Pos -> Pos
(+.) (x,y) (x',y') = (x+x',y+y')

(/.) :: Pos -> (Int,Int) -> Pos
(/.) (x,y) (n,m) = (x `div` n,y `div` m)


