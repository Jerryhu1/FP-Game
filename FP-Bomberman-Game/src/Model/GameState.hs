module Model.GameState where

    import System.Random

    import Model.Player
    import Model.Grid
    import Model.Typeclasses.Positioned

    data GameState = GameState {
        player   :: Player,
        grid     :: Grid,
        gen      :: StdGen
        -- explosions :: [Field]
        -- enemies :: [Player]
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

    checkField :: Player -> Field -> Bool
    checkField pl f = case playerDirection pl of
                      North | (getY pl == getY f + 50 && (getX pl >= getX f && getX pl < getX f + 49)
                              || (getX pl + 49 >= getX f && getX pl + 49 < getX f + 49)) -> True
                            | otherwise -> False
                      East  | (getX pl + 50) == getX f && (getY pl >= getY f - 49)
                              && (getY pl - 50 <= getY f - 1 ) -> True
                            | otherwise -> False
                      South |  (getY pl - 50 == getY f && (getX pl >= getX f && getX pl < getX f + 49)
                                 || (getX pl + 49 >= getX f && getX pl + 49 < getX f + 49)) -> True
                            | otherwise -> False
                      West  |(getX pl) == getX f + 50 && (getY pl >= getY f - 49)
                               && (getY pl - 50 <= getY f - 1 ) -> True
                            | otherwise -> False

                            {-
    breakBlocks :: [Field] -> Grid -> Grid
    breakBlocks (x:[]) gr = setExplosion x gr
    breakBlocks (x:xs) gr = breakBlocks xs newGrid
                    where newGrid = setExplosion x gr

    setExplosion ::  Grid -> [Field] -> Grid
    setExplosion (x:[]) (y:[]) | gameObject x == MetalBlock         = [y]
                               | getPos x == getPos f  = [x { gameObject = Explosion }]
                               | otherwise                         = [y]
    setExplosion (x:xs) (y:ys) | gameObject x == MetalBlock         = x : setExplosion  xs
                               | getPos x == getPos f  = [x { gameObject = Explosion }]
                               | otherwise                         = x : setExplosion f xs
                               -}