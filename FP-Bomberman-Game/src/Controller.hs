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
                  | keyState gstate == Down         = return $ modPlayer gstate $ checkifMovePlayer gstate
                  | otherwise                       = return $ modBombs gstate setTimer
           pureGs <- gs
           let gsNew = snd $ withRandom (randomR (0,3)) pureGs
           let gsMod = foldl moveEnemy gsNew (enemies gsNew)
           putStrLn( show $  enemies gsNew)
           return gsMod

  printCollision :: GameState -> String
  printCollision gs = show $ checkCollisionField (player gs) (grid gs)

  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gstate = return (inputKey e gstate)
  
  -- Eerste opzet lopende player
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey c Down _ _) gstate
    | c== SpecialKey KeyUp     = modPlayer gstate $ changePlayerDir gstate North
    | c== SpecialKey KeyLeft   = modPlayer gstate $ changePlayerDir gstate West
    | c== SpecialKey KeyDown   = modPlayer gstate $ changePlayerDir gstate South
    | c== SpecialKey KeyRight  = modPlayer gstate $ changePlayerDir gstate East
    | c== Char ','             = modBombs gstate $ addBomb (getGridPos $ player gstate)
  inputKey (EventKey (SpecialKey _) Up _ _) gstate = gstate {keyState = Up}  
  inputKey _ gstate = gstate

  setKeyState :: KeyState -> GameState -> GameState
  setKeyState k gstate = gstate { keyState = k}

  modBombs :: GameState -> (Bombs-> Bombs) -> GameState
  modBombs gstate f = gstate {bombs = f $ bombs gstate}
  
  modGrid :: GameState -> (Grid -> Grid) -> GameState
  modGrid gstate f = gstate { grid = f $ grid gstate, keyState = Down}
  
  modPlayer :: GameState -> (Player -> Player) -> GameState
  modPlayer gstate f = gstate { player = f $ player gstate, keyState = Down}
  
  checkIfPlayerIsAlive :: GameState -> GameState
  checkIfPlayerIsAlive gs | state (player gs) == Alive  = gs
                          | otherwise                   = gs { currentState = GameOver }




