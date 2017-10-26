-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model.GameState
import Model.Grid
import Model.Typeclasses.Positioned
import Model.Player

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [ 
                                drawGrid $ grid gstate,
                                drawPlayer $ player gstate
                           ]

grassColor :: Color
grassColor = green

stoneColor :: Color
stoneColor = greyN 0.5

drawStone :: Picture
drawStone = color stoneColor $ rectangleSolid blockSize blockSize

drawMetalBlock :: Picture
drawMetalBlock = color black $ rectangleSolid blockSize blockSize

drawGrass :: Picture
drawGrass = color green $ rectangleSolid blockSize blockSize

drawPowerUp :: Picture
drawPowerUp = color yellow $ rectangleSolid blockSize blockSize

drawBomb :: Picture
drawBomb = color red $ circleSolid 15

drawPlayer :: Player -> Picture
drawPlayer p = translate' (getPos p) $ color blue $ circleSolid 20

drawField :: Field -> Picture
drawField f = case gameObject f of
                PowerUp -> drawPowerUp
                Empty -> drawGrass
                MetalBlock -> drawMetalBlock
                StoneBlock -> drawStone
                Bomb       -> drawBomb
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

setPosToPixels :: Pos -> Pos
setPosToPixels p = ((-375+xPos ),(375- yPos ))
                        where xPos = fst p
                              yPos = snd p

--Translates a picture by Pos
translate' :: Pos -> Picture -> Picture
translate' p = translate (fromIntegral $ fst p) (fromIntegral $ snd p)

windowSize :: Int
windowSize = 800 

blockSize :: Float
blockSize = 50.0