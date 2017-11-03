module Model.GameState where

    import System.Random
    import Graphics.Gloss.Interface.Pure.Game

    import Model.Player
    import Model.Grid
    import Model.GameObject
    import Model.Typeclasses.Positioned
    import Model.Typeclasses.HasArea
    import Debug.Trace
    
    data GameState = GameState {
        player       :: Player,
        grid         :: Grid,
        currentState :: CurrentState,
        gen          :: StdGen,
        keyState     :: KeyState,
        enemies      :: [Player]
        -- explosions :: [Field]
        -- enemies :: [Player]
    }

    data CurrentState = Loading | Running | Paused | GameOver
            deriving(Show, Eq)    

    initGame :: GameState
    initGame = GameState initPlayer createGrid Loading (mkStdGen 0) Up initEnemies

    getRNumber :: IO Int
    getRNumber = getStdRandom (randomR(1,100))
     
    {-
        Generates random blocks by RNG in Gamestate
        Chance is now set at 60%, should be dynamic
        Needs a limit of amount of blocks?
    -}
    setBreakableBlocks :: GameState -> IO GameState
    setBreakableBlocks gs = 
            do 
                g <- return $ grid gs
                newGrid <- mapM setBreakableBlock g
                return gs { grid = newGrid, currentState = Running}
    
    setBreakableBlock :: Field -> IO Field
    setBreakableBlock f = do
                            rng <- getRNumber
                            let obj = gameObject f
                            return (case obj of 
                                Empty | rng > 70 && not (isSafeZone f)      -> f { gameObject = StoneBlock}
                                      | otherwise                      -> f
                                _                                      -> f )
   
    isSafeZone :: Field -> Bool
    isSafeZone f | pos == (-375, 375) || pos == (-325, 375) || pos == (-375, 325) = True
                 | otherwise = False
                where pos = fieldPosition f                          

    checkIfPlayerCollision :: Player -> Grid -> Bool
    checkIfPlayerCollision _ []     = False
    checkIfPlayerCollision p (x:[])  | gameObject x /= Empty &&  gameObject x /= PowerUp && checkField p x == True  = True
                                     | otherwise                 = False
    checkIfPlayerCollision p (x:xs)  | gameObject x /= Empty &&  gameObject x /= PowerUp && checkField p x == True  = True
                                     | otherwise                 = checkIfPlayerCollision p xs
    checkField :: Player -> Field -> Bool
    checkField pl f =   let (x,y) = getPos pl in
                        case playerDirection pl of
                    North   | inArea f (x,y+1) 
                                || inArea f (x+49,y+1) -> True
                            | otherwise                         -> False
                    East    | inArea f (x+50,y)
                                || inArea f (x+50,y-49) -> True
                            | otherwise                         -> False
                    South   | inArea f (x,y-50)
                                || inArea f (x+49,y-50) -> True
                            | otherwise                         -> False
                    West    | inArea f (x-1,y)
                                || inArea f (x-1,y-49) -> True
                            | otherwise                         -> False

    modPlayer :: GameState -> (Player -> Player) -> GameState
    modPlayer gstate f = gstate { player = f $ player gstate}

    
    checkifMovePlayer :: GameState -> Player -> Player
    checkifMovePlayer gs p  | checkIfPlayerCollision p $ grid gs  = p
                            | otherwise                           = movePlayerInDir p

