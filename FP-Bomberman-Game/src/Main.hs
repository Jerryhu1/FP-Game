module Main where

import Controller
import Model.GameState
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (800, 800) (0, 0)) -- Or FullScreen
              white            -- Background color
              10               -- Frames per second
              initGame         -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
              
              
              
-- startGame :: IO ()
-- startGame =  do putStrLn("Press a key to start the game")
--                 getLine
--                 putStrLn("Enter your name: ")
--                 name <- getLine
--                 let gameState = initGame $ setNewPlayer name
--                 putStr $ show(player gameState)

           