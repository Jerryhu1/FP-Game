module Model.Typeclasses.Positioned where

type Pos = (Int , Int)
type Vel = Int
data Direction = North | South | East | West deriving(Show)

class Positioned a where
    getPos :: a -> Pos



class Movable a where
    setPos :: Pos -> a -> a
    setDir :: Direction -> a -> a


class HasArea a where
    inArea :: a -> Pos -> Bool

(+.) :: Pos -> Pos -> Pos
(+.) (x,y) (x',y') = (x+x',y+y')

(*.) :: Pos -> (Int -> Int) -> Pos
(*.) (x,y) f = (f x,f y)


