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

-- Eerste opzet lopende player
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) _ _ _) gstate
  | c== KeyUp     = movePlayer (0,1) gstate
  | c== KeyLeft   = movePlayer (-1,0) gstate
  | c== KeyDown   = movePlayer (0,-1) gstate
  | c== KeyRight  = movePlayer (1,0) gstate
inputKey _ gstate = gstate 


movePlayer :: (Int,Int) -> GameState -> GameState
movePlayer (x,y) gstate = let   orgPlayer = player gstate
                                (p1,p2) = getPos orgPlayer                              
                          in    gstate {player = orgPlayer {playerPosition = (p1+x,p2+y) } }
                          --    gstate {player = setPos (getPos player + dir) player }
                          
--modPlayer :: (Player -> Player) -> GameState -> GameState










{-

--nu met Lenses
inputKey' :: Event -> GameState -> GameState
inputKey' (EventKey (SpecialKey c) _ _ _) gstate
  | c== KeyUp     = player.playerPosition._2 +~ 1 $ gstate
  | c== KeyLeft   = player.playerPosition._1 -~ 1 $ gstate
  | c== KeyDown   = player.playerPosition._2 -~ 1 $ gstate
  | c== KeyRight  = player.playerPosition._1 +~ 1 $ gstate
inputKey' _ gstate = gstate -- Otherwise keep the same

-}





