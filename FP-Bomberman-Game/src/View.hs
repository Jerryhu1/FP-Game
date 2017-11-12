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
import Model.Log

view :: GameState -> IO Picture
view gs = do
            if(currentState gs == Running) then return $ viewPure gs
            else do
                hs <- drawHighScore
                let pics = pictures [viewPure gs, hs]
                return pics

drawHighScore :: IO Picture
drawHighScore = do
                    highScore <- readCurrentHighscore
                    let showScore = translate' (180, -100) $ scale 0.5 0.5 $ color white $ text highScore
                    return showScore

viewPure :: GameState -> Picture
viewPure gstate  = pictures [
                                drawBG,
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
drawBomb b = render b

drawExplosion :: Explosion -> Picture
drawExplosion b =  color (dark red) $ rectangleSolid blockSize blockSize


setPosToPixels :: Pos -> Pos
setPosToPixels p = ((-375+xPos ),(375- yPos ))
                        where xPos = fst p
                              yPos = snd p

drawBG :: Picture
drawBG = translate' (-25, 75) $ png "res/bg.png"

blockSize :: Float
blockSize = 50.0

