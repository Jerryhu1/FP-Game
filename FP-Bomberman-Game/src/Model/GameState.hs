module Model.GameState where

    import System.Random
    import Graphics.Gloss.Game

    import Model.Typeclasses.Renderizable

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
    instance Renderizable GameState where
        render gs = pictures[ render $ player gs, pictures (map render (enemies gs) ), pictures ( map render (bombs gs) ) ]

    data CurrentState = Loading | Running | Paused | GameOver
            deriving(Show, Eq)    

    initGame :: GameState
    initGame = GameState initPlayer level1 Loading (mkStdGen 0) Up initEnemies []


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
    checkCollision pl a = let (x',y') = getPos pl 
                              (x,y) = (x'+10, y'-10)
                              (w,h) = (playerWidth, playerHeight) in
                          case playerDirection pl of
                                North   | inArea a (x,y+1)
                                            || inArea a (x+w,y+1) -> True
                                        | otherwise                         -> False
                                East    | inArea a (x+50,y)
                                            || inArea a (x+50,y-h) -> True
                                        | otherwise                         -> False
                                South   | inArea a (x,y-50)
                                            || inArea a (x+w,y-50) -> True
                                        | otherwise                         -> False
                                West    | inArea a (x-1,y)
                                            || inArea a (x-1,y-h) -> True
                                        | otherwise                         -> False

                            

    modBombs :: GameState -> (Bombs-> Bombs) -> GameState
    modBombs gstate f = gstate {bombs = f $ bombs gstate}
  
    modGrid :: GameState -> (Grid -> Grid) -> GameState
    modGrid gstate f = gstate { grid = f $ grid gstate}
  
    modPlayer :: GameState -> (Player -> Player) -> GameState
    modPlayer gstate f = gstate { player = f $ player gstate}



    --COLLISION DETECTION --
    --BOMBS VS GRID--

    
    modifyBombs :: GameState -> GameState
    modifyBombs gs | length explodingBombs>0   = modifyGrid $ modifyPlayer $ modifyBombs gs
                   | otherwise                 = modifyBombs gs
            where modifyPlayer = \gs -> modPlayer gs $ checkCollisionBombs explodingBombs
                  modifyGrid = \gs -> modGrid gs $ checkDestructionBlocks explodingBombs
                  modifyBombs = \gs -> modBombs gs setTimer
                  newBombs = setTimer $ bombs gs
                  explodingBombs = filter (\b -> bombStatus b == Exploding) newBombs
    
    checkDestructionBlocks :: Bombs -> Grid -> Grid
    checkDestructionBlocks b grid = foldl checkDestruction grid b

    checkDestruction :: Grid -> Bomb -> Grid
    checkDestruction [] b = []
    checkDestruction (x:xs) b | gameObject x == StoneBlock && inArea b (getPos x) = checkDestruction xs b 
                              | otherwise                                         = x : checkDestruction xs b 
  
    

       
    --BOMBS VS PLAYER--    
    checkCollisionBombs :: Bombs -> Player -> Player
    checkCollisionBombs [] p     = p
    checkCollisionBombs (x:xs) p | checkCollision p x            = checkCollisionBombs xs $ playerCollisionBomb p
                                 | otherwise                     = checkCollisionBombs xs p
 
    playerCollisionBomb:: Player -> Player 
    playerCollisionBomb pl = pl { health = Dead}



    --PLAYERS VS GRID--
    --change the direction in which the player is positioned and possibly move player in that direction
    changePlayerDir :: GameState -> Direction -> Player -> Player
    changePlayerDir gstate dir player' = checkifMovePlayer gstate $ setDir dir player'

    checkifMovePlayer :: GameState -> Player -> Player
    checkifMovePlayer gs p  | checkCollisionField p $ grid gs     = newP
                            | otherwise                           = movePlayerInDir newP
            where newP = checkCollisionBombs (bombs gs) p


    checkCollisionField :: Player -> Grid -> Bool
    checkCollisionField _ []     = False
    checkCollisionField p (x:xs)  | gameObject x /= Empty &&  gameObject x /= PowerUp && checkCollision p x  = True
                                  | otherwise                 = checkCollisionField p xs
                            
 



