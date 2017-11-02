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
                  | keyState gstate == Down         = return $ modPlayer gstate $ checkifMovePlayer gstate
                  | otherwise                       = return modGrid gstate 
           g <- gs
           putStrLn $ (printCollision gstate ++ show (player gstate) ++ show secs)
           return $ gstate { elapsedTime = elapsedTime gstate + secs }
  
  updateBombs :: Grid -> Grid
  updateBombs [] = []
  updateBombs (x:xs)  | gameObject x == Bomb    = setTimer x : updateBombs xs
                      | otherwise               = updateBombs xs
                      
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
  
  
  
  
  
  
