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
import Model.Log

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
                        gs <- handleGameState gstate
                        return $ handleAnimation gs


handleGameState :: GameState -> IO GameState
handleGameState gstate   | currentState gstate == Loading   = return $ gstate {currentState = Running} --setBreakableBlocks gstate
                         | currentState gstate == Running   = return $ updateDynamics $ updateElapsedTime gstate
                         | otherwise                        = do writeNewHighScore $ calculateScore gstate
                                                                 return gstate

handleAnimation :: GameState -> GameState
handleAnimation gstate = checkPlayerVictory $ checkIfPlayerIsAlive $  modPlayer (modEnemies gstate animatePlayer) animatePlayer

updateDynamics:: GameState -> GameState
updateDynamics gstate | keyState gstate == Down   = update . modPlayer gstate $ checkifMovePlayer gstate
                      | otherwise                 = update gstate
      where update = moveEnemies . createRandomness . modifyDynamics
            createRandomness = \gs ->  snd $ withRandom (randomR (0,3)) gs
            moveEnemies = \gs -> foldl moveEnemy gs (enemies gs)
            --functies moveEnemies + createRandomness staan beter in een nieuwe functie modEnemies
            --zie functie modPlayer
            --is er geen andere manier om randomness te maken?

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput ev gs
        | currentState gs == Running && playerState /= Dying = inputKeyRunning ev gs
        | currentState gs == Paused                          = inputKeyPaused ev gs
        | currentState gs == GameOver                        = inputKeyMenu ev gs
        | currentState gs == Victory                         = inputKeyMenu ev gs
        | otherwise                                          = gs
            where playerState = state $ player gs

-- Handles the input when gamestate is running. Changes the state of the player
inputKeyRunning :: Event -> GameState -> GameState
inputKeyRunning (EventKey c Down _ _) gstate
    | c == SpecialKey KeyUp     = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate North
    | c == SpecialKey KeyLeft   = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate West
    | c == SpecialKey KeyDown   = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate South
    | c == SpecialKey KeyRight  = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate East
    | c == SpecialKey KeyEsc    = gstate { currentState = Paused }
    | c == Char ',' = modBombs gstate $ addBombs (getGridPos $ player gstate)
inputKeyRunning (EventKey (SpecialKey _) Up _ _) gstate = setPlayerState Idle $ gstate {keyState = Up}
inputKeyRunning _ gstate = gstate

-- Handles the input when the game is paused
inputKeyPaused :: Event -> GameState -> GameState
inputKeyPaused (EventKey c Down _ _) gstate
                | c == SpecialKey KeyEsc        = gstate { currentState = Running }
                | c == SpecialKey KeySpace      = error "Quit the game"
inputKeyPaused _ gstate = gstate

-- Handles the input when the game is over
inputKeyMenu :: Event -> GameState -> GameState
inputKeyMenu (EventKey c Down _ _) gstate
                | c == SpecialKey KeyEsc       = undefined
                | c == SpecialKey KeySpace     = initGame
inputKeyMenu _ gstate = gstate
