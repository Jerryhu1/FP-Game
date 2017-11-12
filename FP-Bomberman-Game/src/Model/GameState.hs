module Model.GameState where
    
    import System.Random
    import Graphics.Gloss.Game
    import Data.List (elemIndex)
    import Data.Maybe (fromJust)
    
    import Model.Typeclasses.Renderizable
    import Model.Typeclasses.Positioned
    
    import Model.Player
    import Model.PowerUp
    import Model.Grid
    import Model.Bombs
    
    --General--
    data GameState = GameState {
        player       :: Player,
        grid         :: Grid,
        currentState :: CurrentState,
        gen          :: StdGen,
        keyState     :: KeyState,
        enemies      :: Enemies,
        bombs        :: Bombs,
        explosions   :: [Explosion],
        powerUps  :: [PowerUp],
        elapsedTime :: Float
    }
    
    type Enemies = [Player]
    
    instance Renderizable GameState where
        render gs
                  | currentState gs == Paused   = pictures (pics ++ [png "res/pause-screen.png"] )
                  | currentState gs == GameOver = pictures (pics ++ [png "res/lose-screen.png", score])
                  | currentState gs == Victory  = pictures (pics ++ [png "res/victory-screen.png", score])
                  | otherwise = pictures pics
                  where pics = [ render $ player gs
                                 , pictures (map render (enemies gs) )
                                 , pictures (map render (bombs gs) )
                                 , pictures (map render (explosions gs)) 
                                 , pictures  (map render (powerUps gs))
                                 , time ]
                        score = translate' (200, 160) $ scale 0.5 0.5 $ color white $ text (show $ calculateScore gs)
                        time  = translate' (325, 475) $ scale 0.2 0.2 $ text (take (formatTime + 3) showTime)
                        formatTime = fromJust (elemIndex '.' showTime)
                        showTime = "Time: " ++ show (elapsedTime gs)


    data CurrentState = Loading | Running | Paused | GameOver | Victory
            deriving(Show, Eq)
    
    initGame :: GameState
    initGame = GameState initPlayer level1 Loading (mkStdGen 0) Up initEnemies [] [] [] 0.0

    
    --Main Modification functions--
    setKeyState :: KeyState -> GameState -> GameState
    setKeyState Down gstate = gstate { keyState = Down, player = setPlayerState Walking $ player gstate}
    setKeyState Up gstate = gstate { keyState = Up, player = setPlayerState Idle $ player gstate}
    

    checkIfDropBomb :: GameState -> Player -> GameState
    checkIfDropBomb gstate pl | canDropBomb pl  = modPlayer (modBombs gstate $ addBombs $ getGridPos pl) setTimerPlayer
                              | otherwise       = gstate

    modBombs:: GameState -> (Bombs -> Bombs) -> GameState
    modBombs gstate f = gstate { bombs = f $ bombs gstate}
    
    modPlayer :: GameState -> (Player -> Player) -> GameState
    modPlayer gstate f = gstate { player = f $ player gstate}

    modEnemies :: GameState -> (Player -> Player) -> GameState
    modEnemies gstate f = gstate {enemies = map f (enemies gstate) }



    -- UPDATING GAMESTATE--

    --Main fuction for updating the GameState--
    --If there are no explosions, just update the bomb timers and powerups
    --If there are explosions, check collision between explosions and grid, powerups, enemies and player
    modifyDynamics :: GameState -> GameState
    modifyDynamics gs | not(null (explosions gs))       = updateRNG $ update $ modifyBombs gs
                      | otherwise                       =  modifyBombs gs
                    where update = modifyPlayer . modifyEnemies . modifyPowerUps . modifyGrid 
                          modifyPlayer g = modPlayer g $ checkCollisionExplosions $ explosions g
                          modifyEnemies g = modEnemies g $ checkCollisionExplosions $ explosions g
                          modifyGrid g = g {grid = foldl checkDestruction (grid g) (explosions g)}
                          modifyPowerUps g =  g {powerUps = foldl checkPowerDestruction (powerUps g) (explosions g)}

    updateRNG :: GameState -> GameState
    updateRNG gs = snd $ withRandom next gs
    
    --Updates timers in bombs and explosions, and creates new ones.
    --Also checks if any Powerups have been picked up by Player and removes these
    modifyBombs :: GameState -> GameState                      
    modifyBombs gs = gs {bombs = newBombs, explosions = explosionsTot, powerUps = powerUpsTot, player = timerCountDownPlayer newPlayer}
            where (newBombs, newExplosions) = setTimerBombs (bombs gs)
                  (newOldExplosions, newPowerUps) = modOldExplosions (grid gs) (gen gs) (explosions gs)
                  explosionsTot = newExplosions ++ newOldExplosions
                  (newOldPowerUps, newPlayer) = checkCollisionPowerUp [] (player gs) $ powerUps gs
                  powerUpsTot = newOldPowerUps ++ newPowerUps
                  
    --Updates timers in Bombs. Also creates explosions when timers run out.
    setTimerBombs :: Bombs -> (Bombs,Explosions)
    setTimerBombs bombs = let      setTimers = map explosionCountDown bombs
                                   newBombs = filter (\b -> timeTillExplosion b > 0) setTimers
                                   explodingBombs = filter (\b -> timeTillExplosion b == 0) setTimers
                                   newExplosions = makeExplosions explodingBombs in
                          (newBombs, newExplosions)
    
    
    --Updates timers in Explosions. Also checks for Collision with Grid
    modOldExplosions :: Grid -> StdGen -> Explosions -> (Explosions,[PowerUp])
    modOldExplosions gr gen explosions = let (newExplosions, newSpeedBoosts) = unzip $ map (checkCollisionEx gen gr) explosions in
                                  (setTimerExplosion newExplosions, concat newSpeedBoosts)
    
    --Checks if an explosion collides with Fields in Grid
    --Uses RNG to randomly drop Powerups when an explosion collides with a stone block
    checkCollisionEx :: StdGen -> Grid -> Explosion -> (Explosion,[PowerUp])
    checkCollisionEx gen [] ex      | explosionStatus ex == Mid = (ex, [])
                                    | otherwise =  (moveExplosion ex,[])
    checkCollisionEx gen (x:xs) ex  | checkCollision ex x && explosionStatus ex /= Moving 
                                            = (ex, []) --explosion cannot move through blocks if it has already destroyed a stone block
                                    | checkCollision ex x && gameObject x == StoneBlock 
                                            = (setExplosionDestructed $ moveExplosion ex, dropPowerUp (snd $ next gen) $ getPos x)
                                            --if explosion is moving and collides with a stoneblock, set status to Destructed and possibly drop PowerUp
                                    | checkCollision ex x        = (ex, [])
                                            -- if explosion collides with a metal block, stop moving
                                    | otherwise                  = checkCollisionEx (snd $ next gen) xs ex
    
    
    dropPowerUp:: StdGen -> Pos -> [PowerUp]
    dropPowerUp gen pos | rng > 70 =  [addNewPowerUp pos gen]
                           | otherwise = []
                            where rng = fst $ randomR (0,100) gen :: Int



    --COLLISION DETECTION --
    
    --MAIN GENERIC COLLISION DETECTION FUNCTION--
    -- Given an object that has an area, and a movable object, return true if there's a collision between these two, else false
    checkCollision :: (HasArea a, Movable b) => b -> a -> Bool
    checkCollision pl a = let (x,y) = getPos pl
                              (w,h) = (width pl, height pl) in
                          case getDir pl of
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
    
    
    
    --EXPLOSIONS VS GRID--

    --If Explosion collides with stoneBlock, remove stoneblock from grid
    checkDestruction :: Grid -> Explosion -> Grid
    checkDestruction [] b = []
    checkDestruction (x:xs) b | gameObject x == StoneBlock && checkCollision b x  = xs
                              | otherwise                                         = x : checkDestruction xs b
    
    --If Explosion collides with a powerup, remove powerup
    checkPowerDestruction :: [PowerUp] -> Explosion -> [PowerUp]
    checkPowerDestruction [] b = []
    checkPowerDestruction (x:xs) b | explosionStatus b == Moving && inArea b (getPos x) = xs
                                   | otherwise           = x : checkPowerDestruction xs b
    
    --EXPLOSIONS VS PLAYER--

    --If Explosions encounter a Player, set that players' status to dead
    checkCollisionExplosions :: Explosions -> Player -> Player
    checkCollisionExplosions [] p     = p
    checkCollisionExplosions (x:xs) p   | checkCollision p x                   = setPlayerDead p
                                        | otherwise                            = checkCollisionExplosions xs p
    
                                        
    setPlayerDead :: Player -> Player
    setPlayerDead pl = pl { state = Dying}
    
    
    
    --PLAYERS MOVEMENT--

    --change the direction in which the player is positioned and possibly move player in that direction
    changePlayerDir :: GameState -> Direction -> Player -> Player
    changePlayerDir gstate dir player' = checkIfMovePlayer gstate $ setDir dir player'

    
    -- If player collides with an enemy, set players' status to Dying.
    -- If player collides with a block, don't change position. Otherwise move
    checkIfMovePlayer :: GameState -> Player -> Player
    checkIfMovePlayer gs p  | checkCollisionSurr p aliveEnemies        = setPlayerDead p
                            | checkCollisionSurr p $ bombs gs          = p
                            | checkCollisionSurr p $ grid gs           = p
                            | otherwise                                = movePlayerInDir p
                            where aliveEnemies = filter (\x -> health x == Alive) (enemies gs)
                                    


    --Checks if Player collides with an enemy
    checkCollisionSurr :: HasArea a => Player -> [a] -> Bool
    checkCollisionSurr p [] = False
    checkCollisionSurr p (x:xs) | checkCollision p x     = True
                                | otherwise              = checkCollisionSurr p xs
                                   


    -- Applies an effect/powerup on the player if there is a collision, otherwise returns the original player
    checkCollisionPowerUp ::  [PowerUp] -> Player -> [PowerUp] -> ([PowerUp],Player)
    checkCollisionPowerUp acc pl [] = (acc,pl)
    checkCollisionPowerUp acc pl b@(x:xs) | checkCollision pl x = (acc++xs,applyEffectOnPlayer x pl)
                                          | otherwise           = checkCollisionPowerUp (x:acc) pl xs



    --CURRENT STATE FUNCTIONS--
    --END OF GAME--

    checkIfPlayerIsAlive :: GameState -> GameState
    checkIfPlayerIsAlive gs | health (player gs) == Alive  = gs
                            | otherwise                    = gs { currentState = GameOver }

    checkPlayerVictory :: GameState -> GameState
    checkPlayerVictory gs | allDead     = gs { currentState = Victory }
                          | otherwise   = gs
                where allDead = all ( == Dead) (map health (enemies gs))

    checkIfEnemiesLeft :: GameState -> GameState
    checkIfEnemiesLeft gs | not (any ( == Alive ) (map health (enemies gs )))  = gs
                          | otherwise                         = gs { currentState = GameOver}

    updateElapsedTime :: GameState -> GameState
    updateElapsedTime gs = gs {elapsedTime = elapsedTime gs + 0.16}

    -- Calculates a score based on elapsed time
    calculateScore :: GameState -> Int
    calculateScore gs | currentState gs == Victory =  round (20000.0 - (elapsedTime gs * 100.0))
                      | otherwise                  =  round (0.0 + (elapsedTime gs * 10.0))


    --CREATE RANDOM NUMBERS--

    --Used for enemy movement and powerups
    withRandom :: (StdGen -> (Int, StdGen)) -> GameState -> (Int, GameState)
    withRandom f gs = let (res, g') = f (gen gs)
                        in ( res, gs { gen = g'} )

    genNumberByRange :: GameState -> (Int,Int) -> (Int, GameState)
    genNumberByRange gs (min,max)
          = let (n, g') = randomR (min,max) (gen gs)
                in (n, gs { gen = g'})





{- These functions are used for generating random maps
                            
    Generates random blocks by RNG in Gamestate
    Chance is now set at 60%
    
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
                                MetalBlock | rng > 70 && not (isSafeZone f)      -> f { gameObject = StoneBlock}
                                      | otherwise                      -> f
                                _                      


                                    
    isSafeZone :: Field -> Bool
    isSafeZone f | pos == (-375, 375) || pos == (-325, 375) || pos == (-375, 325) = True
                 | otherwise = False
                where pos = fieldPosition f
                                -}
