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


    genNumber :: GameState -> (Int , GameState)
    genNumber gs
      = let (n , g') = next (gen gs)
         in (n , gs { gen = g' })

    genNumberByRange :: GameState -> (Int, GameState)
    genNumberByRange gs
      = let (n, g') = randomR (gen gs)
            in (n, gs { gen = g'})
    
    --hier een rng om random blokken in het veld te zetten?

    setNewPlayer :: String -> Player
    setNewPlayer name = Player name 100 (0,0) 1 "test"

    
    setBreakableBlocks :: GameState -> Int
    setBreakableBlocks gs = fst $ randomR (0,10) randomNumber
                        where grid' = grid gs
                              randomNumber = fst $ randomR (0,10) gen gs
        
