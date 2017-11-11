module Model.Random where

import Model.GameState

import System.Random

withRandom :: (StdGen -> (Int, StdGen)) -> GameState -> (Int, GameState)
withRandom f gs = let (res, g') = f (gen gs)
                    in ( res, gs { gen = g'} )

genNumberByRange :: GameState -> (Int, GameState)
genNumberByRange gs
      = let (n, g') = randomR (0,3) (gen gs)
            in (n, gs { gen = g'})

getRNumber :: IO Int
getRNumber = getStdRandom (randomR(1,100))
