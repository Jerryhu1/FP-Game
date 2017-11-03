module Model.Typeclasses.Renderizable where

import Graphics.Gloss.Game

import Model.Typeclasses.Positioned

class Renderizable a where
    render :: a -> Picture

--Translates a picture by Pos
translate' :: Pos -> Picture -> Picture
translate' p = translate (fromIntegral $ fst p) (fromIntegral $ snd p)