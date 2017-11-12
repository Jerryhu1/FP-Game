module Model.Typeclasses.Positioned where

type Pos = (Int , Int)
type Vel = Int
data Direction = North | South | East | West deriving(Show, Eq)

class Positioned a where
    getPos :: a -> Pos


class HasArea a => Movable a where
    setPos :: Pos -> a -> a
    setDir :: Direction -> a -> a
    getDir :: a -> Direction


class Positioned a => HasArea a where
    inArea :: a -> Pos -> Bool
    width :: a -> Int
    height :: a -> Int




