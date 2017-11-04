-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss.Game


import Model.GameState
import Model.Grid
import Model.GameObject
import Model.Typeclasses.Positioned
import Model.Typeclasses.Renderizable
import Model.Player

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [ 
                                drawGrid $ grid gstate,
                                render gstate
                           ]

drawField :: Field -> Picture
drawField f = render $ gameObject f
{-
    Takes a grid and draws rectangles on the corresponding positions
    1. Get all the positions in pixels
    2. Draw a box and translate it with each position
    3. Return pictures
-}
drawGrid :: Grid -> Picture
drawGrid grid = pictures $ map drawBox grid
                where fieldToDraw = map drawField grid
                      drawBox field = translate' (getPos field) (drawField field)

drawBombs :: Bombs -> Picture
drawBombs bombs = pictures $ map drawBombs bombs
    where drawBombs bomb = translate' (getPos bomb) (drawBomb bomb)

drawBomb :: Bomb -> Picture
drawBomb b  | bombStatus b == UnExploded    = render b
            | bombStatus b == Exploding     = drawExplosion b

drawExplosion :: Bomb -> Picture
drawExplosion b =   let r = fromIntegral $ explosionRadius b in
                    color (dark red) $ rectangleSolid (blockSize*2*r) (blockSize*2*r)


setPosToPixels :: Pos -> Pos
setPosToPixels p = ((-375+xPos ),(375- yPos ))
                        where xPos = fst p
                              yPos = snd p


blockSize :: Float
blockSize = 50.0

grassColor :: Color
grassColor = green

stoneColor :: Color
stoneColor = dark orange

drawStone :: Picture
drawStone = color stoneColor $ rectangleSolid blockSize blockSize

drawMetalBlock :: Picture
drawMetalBlock = color (greyN 0.5) $ rectangleSolid blockSize blockSize

drawGrass :: Picture
drawGrass = color green $ rectangleSolid blockSize blockSize

drawPowerUp :: Picture
drawPowerUp = color yellow $ rectangleSolid blockSize blockSize

drawEnemy :: Player -> Picture
drawEnemy p = translate' (getPos p) $ color (dark red) $ rectangleSolid blockSize blockSize


