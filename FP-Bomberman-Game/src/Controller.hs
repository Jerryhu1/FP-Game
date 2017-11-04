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
                  | otherwise                       = return $ updateDynamics gstate 
           pureGs <- gs
           putStrLn( show $  player pureGs)
           return pureGs

  updateDynamics:: GameState -> GameState
  updateDynamics gstate | keyState gstate == Down   = update . modPlayer gstate $ checkifMovePlayer gstate
                        | otherwise                 = update gstate
          where update = moveEnemies . createRandomness . modifyDynamics
                createRandomness = \gs ->  snd $ withRandom (randomR (0,3)) gs
                moveEnemies = \gs -> foldl moveEnemy gs (enemies gs)
                --functies moveEnemies + createRandomness staan beter in een nieuwe functie modEnemies
                --zie functie modPlayer
                --is er geen andere manier om randomness te maken?

  printCollision :: GameState -> String
  printCollision gs = show $ checkCollisionField (player gs) (grid gs)

  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gstate = return (inputKey e gstate)
  
  -- Eerste opzet lopende player
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey c Down _ _) gstate
    | c== SpecialKey KeyUp     = setKeyState Down $ modPlayer gstate $ changePlayerDir gstate North
    | c== SpecialKey KeyLeft   = setKeyState Down $ modPlayer gstate $ changePlayerDir gstate West
    | c== SpecialKey KeyDown   = setKeyState Down $ modPlayer gstate $ changePlayerDir gstate South
    | c== SpecialKey KeyRight  = setKeyState Down $ modPlayer gstate $ changePlayerDir gstate East
    | c== Char ','             = modDynamics gstate $ addBombs (getGridPos $ player gstate)
  inputKey (EventKey (SpecialKey _) Up _ _) gstate = gstate {keyState = Up}  
  inputKey _ gstate = gstate
  


  setKeyState :: KeyState -> GameState -> GameState
  setKeyState k gstate = gstate { keyState = k}


  
  checkIfPlayerIsAlive :: GameState -> GameState
  checkIfPlayerIsAlive gs | state (player gs) == Alive  = gs
                          | otherwise                   = gs { currentState = GameOver }




