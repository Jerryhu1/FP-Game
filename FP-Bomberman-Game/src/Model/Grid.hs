module Model.Grid where

import Graphics.Gloss.Game
    

import Model.Typeclasses.Positioned
import Model.Typeclasses.Renderizable
import System.Random

data GameObject =   MetalBlock| StoneBlock
    deriving(Show, Ord, Eq)

instance Renderizable GameObject where
    render MetalBlock = png "res/metal-block.png"
    render StoneBlock = png "res/stone-block.png"


data Field = Field {
    fieldPosition :: Pos,
    gameObject :: GameObject
} deriving (Eq)

instance Show Field where
    show f = show(fieldPosition f) ++ " Object: " ++ show(gameObject f)

instance Renderizable Field where
    render f = translate' (getPos f) (render $ gameObject f)  

fieldSize :: Int
fieldSize = 50

type Grid = [Field]

instance Positioned Field where
     getPos = fieldPosition


instance HasArea Field where
    width f = 49
    height f = 49
    inArea f (x,y) = x1 <= x && x <= x2 && y2 <= y && y <= y1
        where   (x1,y1) = getPos f
                (x2,y2) = (x1+ width f,y1-height f)
                


level1 :: Grid
level1 = [
    Field (-425, 425) MetalBlock,
    Field (-375, 425) MetalBlock,
    Field (-325, 425) MetalBlock,
    Field (-275, 425) MetalBlock,
    Field (-225, 425) MetalBlock,
    Field (-175, 425) MetalBlock,
    Field (-125, 425) MetalBlock,
    Field (-75, 425) MetalBlock,
    Field (-25, 425) MetalBlock,
    Field (25, 425) MetalBlock,
    Field (75, 425) MetalBlock,
    Field (125, 425) MetalBlock,
    Field (175, 425) MetalBlock,
    Field (225, 425) MetalBlock,
    Field (275, 425) MetalBlock,
    Field (325, 425) MetalBlock,
    Field (375, 425) MetalBlock,
    --End row 1
    Field (-425, 375) MetalBlock,
    Field (-275, 375) StoneBlock,
    Field (-225, 375) StoneBlock,
    Field (-175, 375) StoneBlock,
    Field (-125, 375) StoneBlock,
    Field (25, 375) StoneBlock,
    Field (125, 375) StoneBlock,
    Field (175, 375) StoneBlock,
    Field (375, 375) MetalBlock,
    -- end row 2
    Field (-425, 325) MetalBlock,
    Field (-325, 325) MetalBlock,
    Field (-275, 325) StoneBlock,
    Field (-225, 325) MetalBlock,
    Field (-175, 325) StoneBlock,
    Field (-125, 325) MetalBlock,
    Field (-75, 325) StoneBlock,
    Field (-25, 325) MetalBlock,
    Field (75, 325) MetalBlock,
    Field (125, 325) StoneBlock,
    Field (175, 325) MetalBlock,
    Field (225, 325) StoneBlock,
    Field (275, 325) MetalBlock,
    Field (375, 325) MetalBlock,
    --end row 3
    Field (-425, 275) MetalBlock,
    Field (-375, 275) StoneBlock,
    Field (-325, 275) StoneBlock,
    Field (-275, 275) StoneBlock,
    Field (-125, 275) StoneBlock,
    Field (-75, 275) StoneBlock,
    Field (25, 275) StoneBlock,
    Field (175, 275) StoneBlock,
    Field (225, 275) StoneBlock,
    Field (275, 275) StoneBlock,
    Field (375, 275) MetalBlock,
    -- end row 4
    Field (-425, 225) MetalBlock,
    Field (-375, 225) StoneBlock,
    Field (-325, 225) MetalBlock,
    Field (-275, 225) StoneBlock,
    Field (-225, 225) MetalBlock,
    Field (-175, 225) StoneBlock,
    Field (-125, 225) MetalBlock,
    Field (-25, 225) MetalBlock,
    Field (25, 225) StoneBlock,
    Field (75, 225) MetalBlock,
    Field (175, 225) MetalBlock,
    Field (275, 225) MetalBlock,
    Field (325, 225) StoneBlock,
    Field (375, 225) MetalBlock,
    -- end row 5
    Field (-425, 175) MetalBlock,
    Field (-325, 175) StoneBlock,
    Field (-125, 175) StoneBlock,
    Field (-25, 175) StoneBlock,
    Field (75, 175) StoneBlock,
    Field (125, 175) StoneBlock,
    Field (175, 175) StoneBlock,
    Field (275, 175) StoneBlock,
    Field (375, 175) MetalBlock,
    -- end row 6
    Field (-425, 125) MetalBlock,
    Field (-375, 125) StoneBlock,
    Field (-325, 125) MetalBlock,
    Field (-275, 125) StoneBlock,
    Field (-225, 125) MetalBlock,
    Field (-175, 125) StoneBlock,
    Field (-125, 125) MetalBlock,
    Field (-75, 125) StoneBlock,
    Field (-25, 125) MetalBlock,
    Field (25, 125) StoneBlock,
    Field (75, 125) MetalBlock,
    Field (175, 125) MetalBlock,
    Field (275, 125) MetalBlock,
    Field (325, 125) StoneBlock,
    Field (375, 125) MetalBlock,
    -- end row 7
    Field (-425, 75) MetalBlock,
    Field (-375, 75) StoneBlock,
    Field (-325, 75) StoneBlock,
    Field (-275, 75) StoneBlock,
    Field (-125, 75) StoneBlock,
    Field (25, 75) StoneBlock,
    Field (125, 75) StoneBlock,
    Field (275, 75) StoneBlock,
    Field (325, 75) StoneBlock,
    Field (375, 75) MetalBlock,
    -- end row 8
    Field (-425, 25) MetalBlock,
    Field (-375, 25) StoneBlock,
    Field (-325, 25) StoneBlock,
    Field (-275, 25) StoneBlock,
    Field (-175, 25) StoneBlock,
    Field (-75, 25) StoneBlock,
    Field (-25, 25) StoneBlock,
    Field (125, 25) StoneBlock,
    Field (175, 25) StoneBlock,
    Field (225, 25) StoneBlock,
    Field (325, 25) StoneBlock,
    --end row 9
    Field (-425, 25) MetalBlock,
    Field (-375, 25) StoneBlock,
    Field (-325, 25) MetalBlock,
    Field (-275, 25) StoneBlock,
    Field (-225, 25) MetalBlock,
    Field (-125, 25) MetalBlock,
    Field (-75, 25) StoneBlock,
    Field (-25, 25) MetalBlock,
    Field (75, 25) MetalBlock,
    Field (175, 25) MetalBlock,
    Field (225, 25) StoneBlock,
    Field (275, 25) MetalBlock,
    Field (325, 25) StoneBlock,
    Field (375, 25) MetalBlock,
    --end row 10
    Field (-425, -25) MetalBlock,
    Field (-375, -25) StoneBlock,
    Field (-325, -25) StoneBlock,
    Field (-225, -25) StoneBlock,
    Field (-175, -25) StoneBlock,
    Field (-125, -25) StoneBlock,
    Field (-75, -25) StoneBlock,
    Field (-25, -25) StoneBlock,
    Field (125, -25) StoneBlock,
    Field (175, -25) StoneBlock,
    Field (225, -25) StoneBlock,
    Field (275, -25) StoneBlock,
    Field (325, -25) StoneBlock,
    Field (375, -25) MetalBlock,
    --end row 11
    Field (-425, -75) MetalBlock,
    Field (-375, -75) StoneBlock,
    Field (-325, -75) MetalBlock,
    Field (-225, -75) MetalBlock,
    Field (-175, -75) StoneBlock,
    Field (-125, -75) MetalBlock,
    Field (-75, -75) StoneBlock,
    Field (-25, -75) MetalBlock,
    Field (25, -75) StoneBlock,
    Field (75, -75) MetalBlock,
    Field (125, -75) StoneBlock,
    Field (175, -75) MetalBlock,
    Field (275, -75) MetalBlock,
    Field (325, -75) StoneBlock,
    Field (375, -75) MetalBlock,
    --end row 12
    Field (-425, -125) MetalBlock,
    Field (-325, -125) StoneBlock,
    Field (-275, -125) StoneBlock,
    Field (-225, -125) StoneBlock,
    Field (-75, -125) StoneBlock,
    Field (25, -125) StoneBlock,
    Field (125, -125) StoneBlock,
    Field (225, -125) StoneBlock,
    Field (275, -125) StoneBlock,
    Field (325, -125) StoneBlock,
    Field (375, -125) MetalBlock,
    --end row 13
    Field (-425, -175) MetalBlock,
    Field (-325, -175) MetalBlock,
    Field (-275, -175) StoneBlock,
    Field (-225, -175) MetalBlock,
    Field (-125, -175) MetalBlock,
    Field (-75, -175) StoneBlock,
    Field (-25, -175) MetalBlock,
    Field (75, -175) MetalBlock,
    Field (175, -175) MetalBlock,
    Field (225, -175) StoneBlock,
    Field (275, -175) MetalBlock,
    Field (375, -175) MetalBlock,
     --end row 14
    Field (-425, -225) MetalBlock,
    Field (-225, -225) StoneBlock,
    Field (-175, -225) StoneBlock,
    Field (-125, -225) StoneBlock,
    Field (-75, -225) StoneBlock,
    Field (75, -225) StoneBlock,
    Field (125, -225) StoneBlock,
    Field (225, -225) StoneBlock,
    Field (375, -225) MetalBlock,
    --end row 15
    Field (-425, -275) MetalBlock,
    Field (-375, -275) MetalBlock,
    Field (-325, -275) MetalBlock,
    Field (-275, -275) MetalBlock,
    Field (-225, -275) MetalBlock,
    Field (-175, -275) MetalBlock,
    Field (-125, -275) MetalBlock,
    Field (-75, -275) MetalBlock,
    Field (-25, -275) MetalBlock,
    Field (25, -275) MetalBlock,
    Field (75, -275) MetalBlock,
    Field (125, -275) MetalBlock,
    Field (175, -275) MetalBlock,
    Field (225, -275) MetalBlock,
    Field (275, -275) MetalBlock,
    Field (325, -275) MetalBlock,
    Field (375, -275) MetalBlock]