module Main where

import Controller
import Model.GameState
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Game

main :: IO ()
main = playIO (InWindow "Lambda Bomberman!" (1000,1000) (-375,0))-- Or FullScreen
              white        -- Background color
              10               -- Frames per second
              initGame         -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
