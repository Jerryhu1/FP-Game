-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model.Model
import Model.Player
import Model.Typeclasses.Positioned

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate =
    return gstate
    
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  | c=='w' = gstate { player = movedPlayer (0,-1) $ player gstate} -- If the user presses a character key, show that one
  | c=='a' = gstate { player = movedPlayer (-1,0) $ player gstate} -- If the user presses a character key, show that one
  | c=='s' = gstate { player = movedPlayer (0,1) $ player gstate} -- If the user presses a character key, show that one
  | c=='d' = gstate { player = movedPlayer (1,0) $ player gstate} -- If the user presses a character key, show that one
inputKey _ gstate = gstate -- Otherwise keep the same

movedPlayer :: (Int,Int) -> Player -> Player
movedPlayer (x,y) player =  let (p1,p2) = pos player 
                            in player { playerPosition = (p1 + x, p2 +y) }


