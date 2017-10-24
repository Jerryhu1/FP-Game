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
inputKey (EventKey (SpecialKey c) Down _ _) gstate
  | c== KeyUp     = flip modPlayer gstate $ movePlayer (0,-1)
  | c== KeyLeft   = flip modPlayer gstate $ movePlayer (-1,0)
  | c== KeyDown   = flip modPlayer gstate $ movePlayer (0,1)
  | c== KeyRight  = flip modPlayer gstate $ movePlayer (1,0)
inputKey _ gstate = gstate 


movePlayer :: Vel -> Player -> Player
movePlayer vel player' = setPos (addVel vel $ getPos player') player'

addVel :: Vel -> Pos -> Pos
addVel (x,y) (x',y') = (x+x',y+y')

modPlayer :: (Player -> Player) -> GameState -> GameState
modPlayer f gstate = gstate { player = f $ player gstate}
                          









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





