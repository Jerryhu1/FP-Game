module Model.GameState where
    
    import System.Random
    import Graphics.Gloss.Game
    import Data.List
    import Data.Maybe
    
    import Model.Typeclasses.Renderizable
    import Model.Typeclasses.Positioned
    import Model.Typeclasses.Effectable
    
    import Model.Player
    import Model.PowerUp
    import Model.Grid
    import Model.GameObject
    
    data GameState = GameState {
        player       :: Player,
        grid         :: Grid,
        currentState :: CurrentState,
        gen          :: StdGen,
        keyState     :: KeyState,
        enemies      :: Enemies,
        bombs        :: Bombs,
        explosions   :: [Explosion],
        speedBoosts  :: [SpeedBoost],
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
                                 , pictures  (map render (speedBoosts gs))
                                 , time ]
                        score = translate' (200, 160) $ scale 0.5 0.5 $ color white $ text (show $ calculateScore gs)
                        time  = translate' (325, 475) $ scale 0.2 0.2 $ text (take (formatTime + 3) showTime)
                        formatTime = fromJust (elemIndex '.' showTime)
                        showTime = "Time: " ++ show (elapsedTime gs)


    data CurrentState = Loading | Running | Paused | GameOver | Victory
            deriving(Show, Eq)
    
    initGame :: GameState
    initGame = GameState initPlayer level1 Loading (mkStdGen 0) Up initEnemies [] [] initSpeedBoost 0.0
    
    initSpeedBoost :: [SpeedBoost]
    initSpeedBoost = [SpeedBoost (-350, 370) 10]
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
    
    -- Modify the state of a bomb inside the gamestate using a function
    modBombs:: GameState -> (Bombs -> Bombs) -> GameState
    modBombs gstate f = gstate { bombs = f $ bombs gstate}
    
    -- Modify the state of a grid inside the gamestate using a function
    modGrid :: GameState -> (Grid -> Grid) -> GameState
    modGrid gstate f = gstate { grid = f $ grid gstate}
    
    -- Modify the state of a player using a function
    modPlayer :: GameState -> (Player -> Player) -> GameState
    modPlayer gstate f = gstate { player = f $ player gstate}

    modEnemies :: GameState -> (Player -> Player) -> GameState
    modEnemies gstate f = gstate {enemies = map f (enemies gstate) }

    modifyDynamics :: GameState -> GameState
    modifyDynamics gs | length (explosions gs) > 0      = modifyEnemies $ modifyPlayer $ modifyBombs gs
                      | otherwise                       = modifyBombs gs
                    where modifyPlayer = \g -> modPlayer g $ checkCollisionBombs $ explosions g
                          modifyEnemies = \g -> modEnemies g $ checkCollisionBombs $ explosions g
                          
    modifyBombs :: GameState -> GameState                      
    modifyBombs gs = gs {grid = newGrid, bombs = newBombs, explosions = explosionsTot, speedBoosts = speedTot, player=newPlayer}
            where (newBombs, newExplosions) = setTimerBombs (bombs gs)
                  (newOldExplosions, newSpeedBoosts) = modOldExplosions (grid gs) (explosions gs)
                  newGrid = foldl checkDestruction (grid gs) newOldExplosions
                  explosionsTot = newExplosions ++ newOldExplosions
                  speedTot = newSpeedBoosts ++ newOldSpeedBoosts
                  (newOldSpeedBoosts, newPlayer) = checkCollisionPowerup [] (player gs) $ speedBoosts gs  
    
    
    setTimerBombs :: Bombs -> (Bombs,Explosions)
    setTimerBombs bombs = let      setTimers = map explosionCountDown bombs
                                   newBombs = filter (\b -> timeTillExplosion b > 0) setTimers
                                   explodingBombs = filter (\b -> timeTillExplosion b == 0) setTimers
                                   newExplosions = makeExplosions explodingBombs in
                          (newBombs, newExplosions)
    
    
    
    modOldExplosions :: Grid -> Explosions -> (Explosions,[SpeedBoost])
    modOldExplosions gr explosions = let (newExplosions, newSpeedBoosts) = unzip $ map (checkCollisionEx gr) explosions in
                                  (setTimerExplosion newExplosions, concat newSpeedBoosts)
    
    
    
    checkCollisionEx :: Grid -> Explosion -> (Explosion,[SpeedBoost])
    checkCollisionEx [] ex     = (moveExplosion ex,[])
    checkCollisionEx (x:xs) ex | checkCollision ex x && explosionStatus ex == Destructed     = (ex, [])
                               | checkCollision ex x && gameObject x == StoneBlock    = (setExplosionDestructed $ moveExplosion ex, [addNewSpeedBoost $ getPos x])
                               | checkCollision ex x        = (setExplosionDestructed $ moveExplosion ex, [])
                               | otherwise                  = checkCollisionEx xs ex
    {-
    dropSpeedBoost:: Pos -> [SpeedBoost]
    dropSpeedBoost pos = do
                            rng <- getRNumber
                            if rng > 70 then return [addNewSpeedBoost pos] else return []
    getRNumber :: IO Int
    getRNumber = getStdRandom (randomR(1,100))

    -}
    --COLLISION DETECTION --
    --BOMBS VS GRID--
    
    
    --   checkDestructionBlocks :: Explosions -> Grid -> Grid
    --   checkDestructionBlocks ex grid = foldl checkDestruction grid ex
    
    checkDestruction :: Grid -> Explosion -> Grid
    checkDestruction [] b = []
    checkDestruction (x:xs) b | gameObject x == StoneBlock && inArea b (getPos x) = xs
                              | otherwise                                         = x : checkDestruction xs b
    
    
    
    
    --BOMBS VS PLAYER--
    checkCollisionBombs :: Explosions -> Player -> Player
    checkCollisionBombs [] p     = p
    checkCollisionBombs (x:xs) p | checkCollision p x            = checkCollisionBombs xs $ setPlayerDead p
                                 | otherwise                     = checkCollisionBombs xs p
    
    setPlayerDead :: Player -> Player
    setPlayerDead pl = pl { state = Dying}
    
    
    --ENEMIES VS PLAYER--
    checkCollisionEnemies :: Player -> [Player] -> Bool
    checkCollisionEnemies p [] = False
    checkCollisionEnemies p (x:xs) | checkCollision p x     = True
                                   | otherwise               = checkCollisionEnemies p xs
    
    --PLAYERS VS GRID--
    --change the direction in which the player is positioned and possibly move player in that direction
    changePlayerDir :: GameState -> Direction -> Player -> Player
    changePlayerDir gstate dir player' = checkifMovePlayer gstate $ setDir dir player'
    
    -- If player collides with a block, don't change position and check for other collisions, otherwise move and check for other collisions.
    checkifMovePlayer :: GameState -> Player -> Player
    checkifMovePlayer gs p  | checkCollisionEnemies p $ enemies gs      = setPlayerDead p
                            | checkCollisionField p $ grid gs           = p
                            | otherwise                                 = movePlayerInDir p
       

    -- Returns True if there is collision with a gameobject, otherwise False
    checkCollisionField :: Player -> Grid -> Bool
    checkCollisionField _ []     = False
    checkCollisionField p (x:xs)  | checkCollision p x  = True
                                  | otherwise                 = checkCollisionField p xs

                                  
    -- Applies an effect/powerup on the player if there is a collision, otherwise returns the original player
    checkCollisionPowerup ::  [SpeedBoost] -> Player -> [SpeedBoost] -> ([SpeedBoost],Player) 
    checkCollisionPowerup acc pl [] = (acc,pl)
    checkCollisionPowerup acc pl b@(x:xs) | checkCollision pl x = (acc++xs,applyEffectOnPlayer x pl)
                                          | otherwise           = checkCollisionPowerup (x:acc) pl xs

    -- Calculates a score based on elapsed time
    calculateScore :: GameState -> Int
    calculateScore gs | currentState gs == Victory =  round (20000.0 - (elapsedTime gs * 100.0))
                      | otherwise                  =  round (0.0 + (elapsedTime gs * 100.0))