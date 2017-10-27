module Model.GameState where

    import System.Random

    import Model.Player
    import Model.Grid
    import Model.Typeclasses.Positioned

    data GameState = GameState {
        player   :: Player,
        grid     :: Grid,
        gen      :: StdGen  
    }

    initGame :: GameState
    initGame = initGrid $ GameState initPlayer createGrid (mkStdGen 0)

    initGrid :: GameState -> GameState
    initGrid gs = gs { grid = setBreakableBlocks gs (grid gs) }

    genNumber :: GameState -> (Int , GameState)
    genNumber gs
      = let (n , g') = next (gen gs)
         in (n , gs { gen = g' })

    genNumberByRange :: GameState -> (Int, GameState)
    genNumberByRange gs
      = let (n, g') = randomR (0,100) (gen gs)
            in (n, gs { gen = g'})
    {-
        Generates random blocks by RNG in Gamestate
        Chance is now set at 60%, should be dynamic
        Needs a limit of amount of blocks?
    -}
    setBreakableBlocks :: GameState -> Grid -> Grid
    setBreakableBlocks gs (x:[]) | fst rng > 40    = [x { gameObject = StoneBlock }]
                                 | otherwise       = [x]
                                 where rng = genNumberByRange gs
    setBreakableBlocks gs (x:xs) | fst rng > 40 && fieldIsEmpty x
                                              = x { gameObject = StoneBlock } : setBreakableBlocks (snd rng) xs
                                 | otherwise  = x : setBreakableBlocks (snd rng) xs
                                 where rng = genNumberByRange gs

    fieldIsEmpty :: Field -> Bool
    fieldIsEmpty f | gameObject f == Empty    = True
                   | otherwise                = False

    checkIfPlayerCollision :: Player -> Grid -> Bool
    checkIfPlayerCollision p (x:[]) | gameObject x == Empty = False
                                    | gameObject x == MetalBlock = checkField p x
                                    | otherwise = False
    checkIfPlayerCollision p (x:xs) | obj == Empty = checkIfPlayerCollision p xs
                                    | gameObject x == MetalBlock   = if (checkField p x == True) then True else checkIfPlayerCollision p xs
                                    | otherwise = checkIfPlayerCollision p xs
                            where obj = gameObject x

    testField :: Field
    testField = Field (-325, -325) MetalBlock

    testPlayer :: Player
    testPlayer = (Player "Jerry" 100 (-325,-275) 10 North "test")


    -- Still WIP, what happens if the player is right in the center of a block?
    checkField :: Player -> Field -> Bool
    checkField pl f = case direction pl of
                      North -> True
                            | otherwise -> False
                      East  | getX pl == ( (getX f) + 50)  -> True
                            | otherwise -> False
                      South | getY pl == getY f -> True
                            | otherwise -> False
                      West  | getX pl == getX f -> True
                            | otherwise -> False

        {-(getY pl == getY f + 50 && (getX pl >= getX f && getX pl <= getX f + 50)
                                        || (getX pl + 50 >= getX f && getX pl + 50 <= getX f + 50)) -> True-}