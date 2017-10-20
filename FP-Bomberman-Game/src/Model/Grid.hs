module Model.Grid where

    import Model.Typeclasses.Positioned

    data Field = Field {
        fieldPosition :: (Int,Int),
        gameObject :: GameObject
    } deriving (Show)

    type Grid = [Field]

    data Block  = Block {
    }

    data GameObject = PowerUp | MetalBlock | StoneBlock | Empty
         deriving(Show)
    -- Misschien PowerUp onderdeel maken van Metalblock?

    instance Positioned Field where
         pos f = fieldPosition f

    {-
    Creates a grid, since index starts at 0, both index are -1, y always has to be 2 less than x
    TO-DO: Integrate setBlocks with createGrid
    -}
    createGrid :: Int -> Grid
    createGrid n = setBlocks $ [Field (x,y) Empty| y <- [0..n-3], x <- [0..n-1]]


    {-
        If position is uneven, draw a metal block, otherwise grass
    -}
    setBlocks :: Grid -> Grid
    setBlocks (x:[]) | mod (xPos x) 2 > 0 && mod (yPos x) 2 > 0 = [Field (fieldPosition x) MetalBlock]
                     | otherwise = [x]
                     where xPos f = fst $ fieldPosition f
                           yPos f = snd $ fieldPosition f
    setBlocks (x:xs) | mod (xPos x) 2 > 0 && mod (yPos x) 2 > 0 = (Field (fieldPosition x) MetalBlock) : setBlocks xs
                     | otherwise = x : setBlocks xs
                     where xPos f = fst $ fieldPosition f
                           yPos f = snd $ fieldPosition f


