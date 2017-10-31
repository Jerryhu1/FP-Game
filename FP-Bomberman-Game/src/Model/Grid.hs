module Model.Grid where

    import Model.Typeclasses.Positioned
    import Model.Typeclasses.HasArea 
    import System.Random

    data Field = Field {
        fieldPosition :: Pos,
        gameObject :: GameObject
    } deriving (Eq)

    instance Show Field where
        show f = show(fieldPosition f) ++ " Object: " ++ show(gameObject f)

    numGridX :: Int
    numGridX = 15

    numGridY :: Int
    numGridY = 13

    fieldSize :: Int
    fieldSize = 50

    gridSizeX = (numGridX-1) * fieldSize
    gridSizeY = (numGridX-3) * fieldSize


    type Grid = [Field]

    data Block  = Block {}

    data GameObject = PowerUp | MetalBlock | StoneBlock | Bomb | Explosion | Empty
         deriving(Show, Ord, Eq)
    -- Misschien PowerUp onderdeel maken van Metalblock?

    instance Positioned Field where
         getPos f = fieldPosition f
         getX f = fst $ getPos f
         getY f = snd $ getPos f    

    instance HasArea Field where
        inArea f (x,y) = let (x1,y1) = (getX f, getY f )
                             (x2,y2) = (+.) (x1,y1) (49, -49)
                         in x1 <= x && x <= x2 && y2 <= y && y <= y1
     

    {-
    Creates a grid, since index starts at 0, both index are -1, y always has to be 2 less than x
    TO-DO: Integrate setBlocks with createGrid
    -}
    createGrid :: Grid
    createGrid = map setFieldPosToPixels $ setBlocks [Field (x,y) Empty| y <- [0..numGridY-1], x <- [0..numGridX-1]]

    setFieldPosToPixels :: Field -> Field
    setFieldPosToPixels f = Field { fieldPosition = ((-375+50 * xPos ) , (375-50* yPos ) ), gameObject = gameObject f }
                            where xPos = getX f
                                  yPos = getY f
    {-
        If position is uneven, draw a metal block, otherwise grass
    -}
    setBlocks :: Grid -> Grid
    setBlocks []     = []
    setBlocks (x:[]) | odd (getX x) && odd (getY x)  = [Field (getPos x) MetalBlock]
                     | otherwise = [x]
    setBlocks (x:xs) | odd (getX x) && odd (getY x)  = (Field (getPos x) MetalBlock) : setBlocks xs
                     | otherwise = x : setBlocks xs

