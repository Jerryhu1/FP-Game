module Model.Model where


    import System.Random

    import Model.Player
    import Model.Grid
    import Model.Typeclasses.Positioned

    data GameState = GameState {
        player   :: Player,
        grid     :: Grid,
        gen      :: StdGen  
    }

    initPlayer :: Player
    initPlayer = Player "Jerry" 100 (-375,375) 10 "test"

    initGame :: GameState
    initGame = GameState initPlayer createGrid (mkStdGen 0

    initGrid :: GameState -> GameState
    initGrid gs = setBreakableBlocks gs (grid gs)

    genNumber :: GameState -> (Int , GameState)
    genNumber gs
      = let (n , g') = next (gen gs)
         in (n , gs { gen = g' })

    genNumberByRange :: GameState -> (Int, GameState)
    genNumberByRange gs
      = let (n, g') = randomR (0,100) (gen gs)
            in (n, gs { gen = g'})
    
    --hier een rng om random blokken in het veld te zetten?

    setNewPlayer :: String -> Player
    setNewPlayer name = Player name 100 (0,0) 1 "test"

    
    setBreakableBlocks :: GameState -> Grid -> Grid
    setBreakableBlocks gs (x:[]) | fst rng > 40    = [x { gameObject = MetalBlock }]
                                 | otherwise       = [x]
                                 where rng = genNumberByRange gs
    setBreakableBlocks gs (x:xs) | fst rng > 40 && fieldIsEmpty x
                                      = x { gameObject = MetalBlock } : setBreakableBlocks (snd rng) xs
                                 | otherwise       = x : setBreakableBlocks (snd rng) xs
                                 where rng = genNumberByrange gs

    fieldIsEmpty :: Field -> Bool
    fieldIsEmpty f | gameObject f == Empty    = True
                   | otherwise                = False
