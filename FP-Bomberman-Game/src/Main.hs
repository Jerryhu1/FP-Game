module Main where

import Controller
import Model.GameState
import View
import qualified Data.ByteString.Lazy as U

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Game

main :: IO ()
main = do     playIO FullScreen -- Or FullScreen
              white        -- Background color
              10               -- Frames per second
              initGame         -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function


