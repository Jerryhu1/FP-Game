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
           let gs | currentState gstate == Loading  = return gstate {currentState = Running} --setBreakableBlocks gstate
                  | currentState gstate == Paused   = return gstate -- Show pause screen and disable movement
                  | currentState gstate == GameOver = return gstate -- Show gameover screen
                  | otherwise                       = return $ updateDynamics gstate
           pureGs <- gs
           let animateGs = checkIfPlayerIsAlive $ modPlayer pureGs (animatePlayer)
           return animateGs

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
  input e gstate = return (handleInput e gstate)
  
  -- Eerste opzet lopende player
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey c Down _ _) gstate
    | c== SpecialKey KeyUp     = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate North
    | c== SpecialKey KeyLeft   = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate West
    | c== SpecialKey KeyDown   = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate South
    | c== SpecialKey KeyRight  = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate East
    | c== SpecialKey KeyEsc       = gstate { currentState = Paused }
    | c== SpecialKey KeySpace            = modDynamics gstate $ addBombs (getGridPos $ player gstate)
  inputKey (EventKey (SpecialKey _) Up _ _) gstate = setPlayerState Idle $ gstate {keyState = Up}
  inputKey _ gstate = gstate

  handleInput :: Event -> GameState -> GameState
  handleInput ev gs | currentState gs == Running = inputKey ev gs
                    | currentState gs == Paused = inputKeyPaused ev gs
                    | currentState gs == GameOver = inputKeyMenu ev gs
                    | otherwise                 = gs

  inputKeyPaused :: Event -> GameState -> GameState
  inputKeyPaused (EventKey c Down _ _) gstate
    | c== SpecialKey KeyEsc       = gstate { currentState = Running }
  inputKeyPaused _ gstate = gstate

  inputKeyMenu :: Event -> GameState -> GameState
  inputKeyMenu (EventKey c Down _ _) gstate
      | c== Char 'y'       = initGame
      | c== Char 'n'       = error "Much noob very wow"
  inputKeyMenu _ gstate = gstate

  setKeyState :: KeyState -> GameState -> GameState
  setKeyState k gstate = gstate { keyState = k}

  setPlayerState :: PlayerState -> GameState -> GameState
  setPlayerState pState gstate = gstate { player = p { state = pState } }
                                    where p = player gstate

  
  checkIfPlayerIsAlive :: GameState -> GameState
  checkIfPlayerIsAlive gs | health (player gs) == Alive  = gs
                          | otherwise                   = gs { currentState = GameOver }




