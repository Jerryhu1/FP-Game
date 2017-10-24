module Model.Model where

    import Model.Player
    import Model.Grid

    data GameState = GameState {
        player   :: Player,
        grid     :: Grid
    }

    initPlayer :: Player
    initPlayer = Player "Jerry" 100 (0,0) (0,0) "test"

    initGame :: GameState
    initGame = GameState initPlayer $ createGrid 5
    
    --hier een rng om random blokken in het veld te zetten?

    setNewPlayer :: String -> Player
    setNewPlayer name = Player name 100 (0,0) (0,0) "test"

    --asdasdasdasdsd