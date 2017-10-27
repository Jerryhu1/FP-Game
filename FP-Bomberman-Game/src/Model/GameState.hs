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
