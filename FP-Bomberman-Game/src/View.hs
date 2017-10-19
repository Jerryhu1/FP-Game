-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model.Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = color white $ text $ show $ player gstate
