-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model.GameState
import Model.Grid
import Model.GameObject
import Model.Typeclasses.Positioned
import Model.Player

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [ 
                                drawGrid $ grid gstate,
                                drawBombs $ bombs gstate,                                
                                drawPlayer $ player gstate
                           ]
{-case direction p of
                North -> translate' (getPos p) $ color blue $ thickArc 0 180 20 10
                South -> translate' (getPos p) $ color blue $ thickArc 0 180 20 10
                East  -> translate' (getPos p) $ color blue $ thickArc (90) (-270) 20 10
                West  -> translate' (getPos p) $ color blue $ thickArc (-90) (-270) 20 10-}

drawField :: Field -> Picture
drawField f = case gameObject f of
                PowerUp -> drawPowerUp
                Empty -> drawGrass
                MetalBlock -> drawMetalBlock
                StoneBlock -> drawStone
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
    where drawBombs bomb = translate' (getPos bomb) drawBomb
                          
drawBomb :: Picture
drawBomb = color red $ circleSolid 15 

setPosToPixels :: Pos -> Pos
setPosToPixels p = ((-375+xPos ),(375- yPos ))
                        where xPos = fst p
                              yPos = snd p

--Translates a picture by Pos
translate' :: Pos -> Picture -> Picture
translate' p = translate (fromIntegral $ fst p) (fromIntegral $ snd p)

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



drawPlayer :: Player -> Picture
drawPlayer p = translate' (getPos p) $ color blue $ rectangleSolid blockSize blockSize

drawExplosion :: Picture
drawExplosion = color (dark red) $ rectangleSolid blockSize blockSize
