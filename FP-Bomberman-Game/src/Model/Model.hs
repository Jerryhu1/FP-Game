module Model.Model where

    import Model.Player
    import Model.Grid
    import Model.Typeclasses.Positioned

    data GameState = GameState {
        player   :: Player,
        grid     :: Grid
    }

    initPlayer :: Player
    initPlayer = Player "Jerry" 100 (0,0) (0,0) "test"

    initGame :: GameState
    initGame = GameState initPlayer $ createGrid
    
    --hier een rng om random blokken in het veld te zetten?

    setNewPlayer :: String -> Player
    setNewPlayer name = Player name 100 (0,0) (0,0) "test"
