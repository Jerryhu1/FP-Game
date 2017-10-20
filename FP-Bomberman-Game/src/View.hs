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

{-
    Takes a grid and draws rectangles on the corresponding positions
    1. Get all the positions in pixels
    2. Draw a stone and translate it with each position
    3. Return pictures
-}
drawGrid :: Grid -> Picture
drawGrid grid = pictures $ map (\x -> translate (fst x) (snd x) $ drawStone) positions
                where positions = mapPositionToPixels $ grid

--Maps the position of each field in a grid to pixels
mapPositionToPixels :: Grid -> [(Float, Float)]
mapPositionToPixels grid = (map mapField (map fieldPosition grid))

{-
Maps starting from upper left corner which is (0,0). 
Grid size is set at 650. Each field in the grid has a size of 50
-}
mapField :: (Int,Int) -> (Float,Float) 
mapField (x,y) =  (fromIntegral(-375+50*x) , fromIntegral(375-50*y) )      
        
windowSize :: Int
windowSize = 800 

blockSize :: Float
blockSize = 50.0