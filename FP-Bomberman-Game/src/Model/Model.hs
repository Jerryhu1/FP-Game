module Model.Model where

    import Model.Player
    import Model.Grid

    data GameState = GameState {
        player   :: Player,
        grid     :: Grid
    }

    initPlayer :: Player
    initPlayer = Player "Jerry" 100 (0,0) 1.0 Down "test"

    initGame :: GameState
    initGame = GameState initPlayer $ createGrid 5 
    
    createGrid :: Int -> Grid
    createGrid n = [Field (x,y) Empty| x <- [0..n], y <- [0..n]]
    
    --hier een rng om random blokken in het veld te zetten?

    setNewPlayer :: String -> Player
    setNewPlayer name = Player name 100 (0,0) 1.0 Down "test"

    --asdasdasdasdsd