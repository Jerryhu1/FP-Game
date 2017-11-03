module Model.GameState where

    import System.Random
    import Graphics.Gloss.Interface.Pure.Game

    import Model.Player
    import Model.Grid
    import Model.GameObject
    import Model.Typeclasses.Positioned
    import Debug.Trace
    
    data GameState = GameState {
        player       :: Player,
        grid         :: Grid,
        currentState :: CurrentState,
        gen          :: StdGen,
        keyState     :: KeyState,
        enemies      :: [Player],
        bombs        :: [Bomb]
        -- explosions :: [Field]
        -- enemies :: [Player]
    }

    
    data CurrentState = Loading | Running | Paused | GameOver
            deriving(Show, Eq)    

    initGame :: GameState
    initGame = GameState initPlayer createGrid Loading (mkStdGen 0) Up initEnemies []


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


    checkCollision :: HasArea a => Player -> a -> Bool
    checkCollision pl a = let (x,y) = getPos pl in
                          case playerDirection pl of
                                North   | inArea a (x,y+1)
                                            || inArea a (x+49,y+1) -> True
                                        | otherwise                         -> False
                                East    | inArea a (x+50,y)
                                            || inArea a (x+50,y-49) -> True
                                        | otherwise                         -> False
                                South   | inArea a (x,y-50)
                                            || inArea a (x+49,y-50) -> True
                                        | otherwise                         -> False
                                West    | inArea a (x-1,y)
                                            || inArea a (x-1,y-49) -> True
                                        | otherwise                         -> False


    
    checkifMovePlayer :: GameState -> Player -> Player
    checkifMovePlayer gs p  | checkCollisionField p $ grid gs     = p
                            | otherwise                           = movePlayerInDir p

  --TO DO: SAMENVOEGEN--
    checkCollisionBombs :: Bombs -> Player -> Player
    checkCollisionBombs [] p     = p
    checkCollisionBombs (x:xs) p | checkCollision p x            = checkCollisionBombs xs $ playerCollisionBomb (bombStatus x) p
                                 | otherwise                     = checkCollisionBombs xs p


    playerCollisionBomb:: BombStatus -> Player -> Player 
    playerCollisionBomb UnExploded pl = pl 
    playerCollisionBomb Exploding pl = pl { health = (health pl) -1 }


    checkCollisionField :: Player -> Grid -> Bool
    checkCollisionField _ []     = False
    checkCollisionField p (x:xs)  | gameObject x /= Empty &&  gameObject x /= PowerUp && checkCollision p x  = True
                                  | otherwise                 = checkCollisionField p xs
                            
 
    --change the direction in which the player is positioned
    changePlayerDir :: GameState -> Direction -> Player -> Player
    changePlayerDir gstate dir player' = checkifMovePlayer gstate $ setDir dir player'

