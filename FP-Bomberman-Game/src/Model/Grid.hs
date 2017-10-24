module Model.Grid where

    import Model.Typeclasses.Positioned
    import System.Random

    data Field = Field {
        fieldPosition :: Pos,
        gameObject :: GameObject
    } deriving (Show)

    type Grid = [Field]

    data Block  = Block {
    }

    data GameObject = PowerUp | MetalBlock | StoneBlock | Empty
         deriving(Show)
    -- Misschien PowerUp onderdeel maken van Metalblock?

    instance Positioned Field where
         getPos f = fieldPosition f
         getX f = fst $ getPos f
         getY f = snd $ getPos f

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
    setBlocks (x:[]) | mod (getX x) 2 > 0 && mod (getY x) 2 > 0 = [Field (getPos x) MetalBlock]
                     | otherwise = [x]
    setBlocks (x:xs) | mod (getX x) 2 > 0 && mod (getY x) 2 > 0 = (Field (getPos x) MetalBlock) : setBlocks xs
                     | otherwise = x : setBlocks xs
                     
