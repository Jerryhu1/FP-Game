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
        keyState     :: KeyState
        -- explosions :: [Field]
        -- enemies :: [Player]
        }

    data CurrentState = Loading | Running | Paused | GameOver
            deriving(Show, Eq)    

    
    
    initGame :: GameState
    initGame = GameState initPlayer createGrid Loading (mkStdGen 0) Up 

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
                putStrLn(show newGrid)
                return gs { grid = newGrid, currentState = Running}
    
    setBreakableBlock :: Field -> IO Field
    setBreakableBlock f = do
                            rng <- getRNumber
                            let obj = gameObject f
                            return (case obj of 
                                Empty | rng > 90      -> f { gameObject = StoneBlock}
                                      | otherwise     -> f
                                _                     -> f )
   

    testField :: Field
    testField = Field (-325, -325) MetalBlock

    testPlayer :: Player
    testPlayer = (Player "Jerry" 100 (-325,-275) 10 North "test")                                

    checkIfPlayerCollision :: Player -> Grid -> Bool
    checkIfPlayerCollision _ []     = False
    checkIfPlayerCollision p (x:xs)  | gameObject x /= Empty &&  gameObject x /= PowerUp && checkField p x == True  = True
                                  --   | gameObject x == Bomb      = moveBomb p (x:xs)
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
