module Model.Grid where

    import Model.Typeclasses.Positioned
    import System.Random

    data Field = Field {
        fieldPosition :: Pos,
        gameObject :: GameObject
    } deriving (Show, Eq)

    numGridX :: Int
    numGridX = 15

    numGridY :: Int
    numGridY = 13

    fieldSize :: Int
    fieldSize = 50

    gridSizeX = (numGridX-1) * fieldSize
    gridSizeY = (numGridX-3) * fieldSize

    --mogelijk ook toevoegen:
   -- numGridY::Int
   -- numGridY = 13
    
    type Grid = [Field]

    data Block  = Block {
    }

    data GameObject = PowerUp | MetalBlock | StoneBlock | Bomb | Empty
         deriving(Show, Ord, Eq)
    -- Misschien PowerUp onderdeel maken van Metalblock?

    instance Positioned Field where
         getPos f = fieldPosition f
         getX f = fst $ getPos f
         getY f = snd $ getPos f

    {-
    Creates a grid, since index starts at 0, both index are -1, y always has to be 2 less than x
    TO-DO: Integrate setBlocks with createGrid
    -}
    createGrid :: Grid
    createGrid = setBlocks $ [Field (x,y) Empty| y <- [0..numGridX-3], x <- [0..numGridX-1]]


    {-
        If position is uneven, draw a metal block, otherwise grass
    -}
    setBlocks :: Grid -> Grid
    setBlocks (x:[]) | odd (getX x) && odd (getY x)  = [Field (getPos x) MetalBlock]
                     | otherwise = [x]
    setBlocks (x:xs) | odd (getX x) && odd (getY x)  = (Field (getPos x) MetalBlock) : setBlocks xs
                     | otherwise = x : setBlocks xs

    {-
    randomNumber :: Int
    randomNumber = fst $ next range
                   where range = genRange (0,6)

    asdf :: Grid -> IO Grid
    asdf g = 
    setBreakableBlocks :: Grid -> IO Grid 
    setBreakableBlocks = map setBreakableBlock

    setBreakableBlock :: Field -> IO Field
    setBreakableBlock f = do randomNumber <- randomIO
                             let number = randomNumber 6
                             if number > 0 then return $ f {gameObject = StoneBlock}
                             else return f
        -}