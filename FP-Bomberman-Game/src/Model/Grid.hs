module Model.Grid where

    import Model.Typeclasses.Positioned
    import Model.Typeclasses.HasArea 
    import Model.GameObject
    import System.Random


    instance Show Field where
        show f = show(fieldPosition f) ++ " Object: " ++ show(gameObject f)

    numGridX :: Int
    numGridX = 15

    numGridY :: Int
    numGridY = 13

    fieldSize :: Int
    fieldSize = 50




    type Grid = [
        (x,y) | x <- [0 .. numGridX], y <- [0 .. numGridY]
    ]
     

    {-
    Creates a grid, since index starts at 0, both index are -1, y always has to be 2 less than x
    -}

    createGrid :: Grid
    createGrid = map setFieldPosToPixels $ setBlocks

    setFieldPosToPixels :: MetalBlock -> MetalBlock
    setFieldPosToPixels m = m { metalBlockPosition = ((-375+50 * xPos ) , (375-50* yPos ) ) }
                            where xPos = getX m
                                  yPos = getY m
                                  
    {-
        If position is uneven, draw a metal block, otherwise grass
    -}
    setBlocks :: Grid -> Grid
    setBlocks []     = []
    setBlocks (x:xs) | odd (fst x) && odd (snd x)  = (MetalBlock {metalBlockPosition = x}) : setBlocks xs
                     | otherwise = setBlocks xs

