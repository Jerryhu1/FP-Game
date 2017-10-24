-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model.Model
import Model.Grid
import Model.Typeclasses.Positioned
import Model.Player

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [ 
                                drawGrid $ createGrid 13,
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

drawBomb :: Field -> Picture
drawBomb f = translate' pos $ color red $ circleSolid 30
            where pos = fieldPosition $ setFieldPosToPixels f

drawPlayer :: Player -> Picture
drawPlayer p = translate' (getPos p) $ color blue $ circleSolid 20

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
drawGrid grid = pictures $ map drawBox fields
                where fields = setGridToPixels grid
                      fieldToDraw = map drawField grid
                      drawBox field = translate' (getPos field) (drawField field)

--Maps the position of each field in a grid to pixels 1. Map Fields to their positions 2. Map to pixels
setGridToPixels :: Grid -> [Field]
setGridToPixels = map setFieldPosToPixels 


{-
Grid size is set at 650. Each field in the grid has a size of 50
Maps Pos to pixels
-}
setFieldPosToPixels :: Field -> Field
setFieldPosToPixels f = Field { fieldPosition = (fromIntegral(-375+50 * xPos ) , fromIntegral(375-50* yPos ) ), gameObject = gameObject f }
                        where xPos = getX f
                              yPos = getY f

--Translates a picture by Pos
translate' :: Pos -> Picture -> Picture
translate' p = translate (fromIntegral $ fst p) (fromIntegral $ snd p)

windowSize :: Int
windowSize = 800 

blockSize :: Float
blockSize = 50.0