-- | This module defines how the state changes
--   in response to time and user input
module Controller where
  
  import Model.GameState
  import Model.Player
  import Model.Typeclasses.Positioned
  import Model.Grid
  import Model.EnemyLogic
  import Model.Random
  import Model.GameObject

  
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
           let gsNew = snd $ withRandom (randomR (0,3)) pureGs
           let gsMod = foldl moveEnemy gsNew (enemies gsNew)
           putStrLn( show $  enemies gsNew)
           return gsMod

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
    | c== Char ','             = setKeyState Down . modGrid gstate $ addGameObject $ Field {fieldPosition = getGridPos $ player gstate, gameObject = Bomb}
  inputKey (EventKey c Up _ _) gstate = setKeyState Up gstate  
  inputKey _ gstate = gstate 

  setKeyState :: KeyState -> GameState -> GameState
  setKeyState k gstate = gstate { keyState = k}
  
  modGrid :: GameState -> (Grid -> Grid) -> GameState
  modGrid gstate f = gstate { grid = f $ grid gstate}

  
