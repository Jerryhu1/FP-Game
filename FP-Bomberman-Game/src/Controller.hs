-- | This module defines how the state changes
--   in response to time and user input
module Controller where
  
  import Model.GameState
  import Model.Player
  import Model.Typeclasses.Positioned
  import Model.Grid
  
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import System.Random
  
  -- | Handle one iteration of the game
  step :: Float -> GameState -> IO GameState
  step secs gstate =
        do 
           let gs | currentState gstate == Loading  = setBreakableBlocks gstate 
                  | currentState gstate == Paused   = undefined -- Show pause screen and disable movement
                  | currentState gstate == GameOver = undefined -- Show gameover screen
                  | otherwise                       = return $ gstate
           
           pureGs <- gs
           let g = snd $ withRandom (randomR (0,3)) pureGs
           let enemyToMove = head (enemies g)
           newGs <- return $ moveEnemyToPos g enemyToMove $ getPath g enemyToMove
          
           putStrLn ( show $getPath pureGs enemyToMove)
           putStrLn $ (printCollision gstate ++ show (player gstate) ++ show secs)
           return newGs

  printCollision :: GameState -> String
  printCollision gs = show $ checkIfPlayerCollision (player gs) (grid gs)

  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gstate = return (inputKey e gstate)
  
  -- Eerste opzet lopende player
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey c Down _ _) gstate
    | c== SpecialKey KeyUp     = setKeyState Down . modPlayer gstate $ checkifMovePlayer gstate . changePlayerDir North
    | c== SpecialKey KeyLeft   = setKeyState Down . modPlayer gstate $ checkifMovePlayer gstate . changePlayerDir West
    | c== SpecialKey KeyDown   = setKeyState Down . modPlayer gstate $ checkifMovePlayer gstate . changePlayerDir South
    | c== SpecialKey KeyRight  = setKeyState Down . modPlayer gstate $ checkifMovePlayer gstate . changePlayerDir East
    | c== Char ','             = modGrid gstate $ addGameObject $ Field {fieldPosition = getGridPos $ player gstate, gameObject = Bomb}
    | c== Char '.'             = modGrid gstate $ addGameObject $ Field {fieldPosition = getGridPos $ player gstate, gameObject = PowerUp}
  inputKey (EventKey c Up _ _) gstate = setKeyState Up gstate  
  inputKey _ gstate = gstate 
  
  --change the direction in which the player is positioned
  changePlayerDir :: Direction -> Player -> Player
  changePlayerDir dir player' = setDir dir player'

  checkifMovePlayer :: GameState -> Player -> Player
  checkifMovePlayer gs p  | checkIfPlayerCollision p $ grid gs  = p
                          | otherwise                           = movePlayerInDir p                      

  p :: Pos
  p = (325,325)

  t = Player "Monstertje" 100 (325,375) 5 East "test"


  moveEnemyToPos :: GameState -> Player -> Pos -> GameState
  moveEnemyToPos gs p pos | getPos p == pos     = gs
                          | otherwise           = modEnemy gs p $ checkifMovePlayer gs . changePlayerDir (getDirectionFromPos p pos)

  getDirectionFromPos :: Player -> Pos -> Direction
  getDirectionFromPos p pos | getX p > fst pos = East
                            | getX p < fst pos = West
                            | getY p < snd pos = North
                            | getY p > snd pos = South
                            | otherwise        = playerDirection p


  addGameObject :: Field -> Grid -> Grid
  addGameObject newField [] = []
  addGameObject newField (x:xs) | fieldPosition newField == fieldPosition x   = newField : addGameObject newField xs
                                | otherwise                                   = x : addGameObject newField xs
  
  setKeyState :: KeyState -> GameState -> GameState
  setKeyState k gstate = gstate { keyState = k}
  
  modGrid :: GameState -> (Grid -> Grid) -> GameState
  modGrid gstate f = gstate { grid = f $ grid gstate}
  
  modPlayer :: GameState -> (Player -> Player) -> GameState
  modPlayer gstate f = gstate { player = f $ player gstate}
                            
  modEnemy :: GameState -> Player -> (Player -> Player) -> GameState
  modEnemy gstate enemy f = gstate { enemies = acc enemy f (enemies gstate) }
                where acc :: Player -> (Player -> Player) -> [Player] -> [Player]
                      acc enemy f (x:[]) | enemy == x   = [f x]
                                         | otherwise    = error "Enemy doesn't exist?"
                      acc enemy f (x:xs) | enemy == x   = f x : xs
                                         | otherwise    = x : (acc enemy f xs)  
  getPath :: GameState -> Player -> Pos
  getPath gs p | rng == 0  = (getX p + 50, getY p)
               | rng == 1  = (getX p - 50, getY p)
               | rng == 2  = (getX p, getY p + 50)
               | rng == 3  = (getX p, getY p - 50)
               | otherwise = (getX p, getY p)    
        where rng :: Int
              rng = fst $ withRandom (randomR (0,3)) gs 
  
  
  withRandom :: (StdGen -> (Int, StdGen)) -> GameState -> (Int, GameState)
  withRandom f gs = let (res, g') = f (gen gs)
                      in ( res, gs { gen = g'} )
  
  genNumberByRange :: GameState -> (Int, GameState)
  genNumberByRange gs
        = let (n, g') = randomR (0,3) (gen gs)
              in (n, gs { gen = g'})
  
  {-
  
  --nu met Lenses
  inputKey' :: Event -> GameState -> GameState
  inputKey' (EventKey (SpecialKey c) _ _ _) gstate
    | c== KeyUp     = player.playerPosition._2 +~ 1 $ gstate
    | c== KeyLeft   = player.playerPosition._1 -~ 1 $ gstate
    | c== KeyDown   = player.playerPosition._2 -~ 1 $ gstate
    | c== KeyRight  = player.playerPosition._1 +~ 1 $ gstate
  inputKey' _ gstate = gstate -- Otherwise keep the same
  
  -}
  
  
  
  
  
  
