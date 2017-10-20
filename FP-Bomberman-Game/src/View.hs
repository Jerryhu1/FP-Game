-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model.Model
import Model.Grid

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = drawGrid $ createGrid 13

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

drawBomb :: (Int, Int) -> Picture
drawBomb p = translate (fromIntegral $ fst pos) (fromIntegral $ snd pos) $ color red $ circle 30
            where pos = mapField p

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
                where fields = mapPositionToPixels grid
                      fieldToDraw = map drawField grid
                      drawBox field = translate (fromIntegral $ fst $ fieldPosition field) (fromIntegral $ snd $ fieldPosition field) $ drawField field

--Maps the position of each field in a grid to pixels 1. Map Fields to their positions 2. Map to pixels
mapPositionToPixels :: Grid -> [Field]
mapPositionToPixels [] = []
mapPositionToPixels (x:[]) = [(Field (mapField $ fieldPosition x) $ gameObject x)]
mapPositionToPixels grid@(x:xs) = (Field (mapField $ fieldPosition x) $ gameObject x) : mapPositionToPixels xs

{-
Maps starting from upper left corner which is (0,0). 
Grid size is set at 650. Each field in the grid has a size of 50
-}
mapField :: (Int,Int) -> (Int,Int)
mapField (x,y) =  (fromIntegral(-375+50*x) , fromIntegral(375-50*y) )


windowSize :: Int
windowSize = 800 

blockSize :: Float
blockSize = 50.0