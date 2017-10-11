module Main where

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main - playIO (InWindow "Counter") (400, 400) (0,0)
                black            -- Background color
                10               -- Frames per second
                initialState     -- Initial state
                view             -- View function
                input            -- Event function
                step             -- Step function

type GameGrid = [(Int,Int)]

generateGrid :: Int -> GameGrid
generateGrid size = [(x,y) | x <- [0..n], y <- [0..n]]

