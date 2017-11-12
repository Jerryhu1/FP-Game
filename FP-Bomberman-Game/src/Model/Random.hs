module Model.Random where

import Model.GameState

import System.Random

withRandom :: (StdGen -> (Int, StdGen)) -> GameState -> (Int, GameState)
withRandom f gs = let (res, g') = f (gen gs)
                    in ( res, gs { gen = g'} )

genNumberByRange :: GameState -> (Int,Int) -> (Int, GameState)
genNumberByRange gs (min,max)
      = let (n, g') = randomR (min,max) (gen gs)
            in (n, gs { gen = g'})

getRNumber :: IO Int
getRNumber = getStdRandom (randomR(1,100))
