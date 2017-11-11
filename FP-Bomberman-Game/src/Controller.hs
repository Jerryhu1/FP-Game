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
step secs gstate = return $ handleAnimation $ handleGameState gstate

handleGameState :: GameState -> GameState
handleGameState gstate   | currentState gstate == Loading   = gstate {currentState = Running} --setBreakableBlocks gstate
                         | currentState gstate == Running   = updateDynamics gstate
                         | otherwise                        = gstate

handleAnimation :: GameState -> GameState
handleAnimation gstate = checkPlayerVictory $ checkIfPlayerIsAlive $  modPlayer (modEnemies gstate animatePlayer) animatePlayer

updateDynamics:: GameState -> GameState
updateDynamics gstate | keyState gstate == Down   = update . modPlayer gstate $ checkifMovePlayer $ checkCollisionEffect gstate (speedBoosts gstate)
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

-- Eerste opzet lopende player
inputKey :: Event -> GameState -> GameState
inputKey (EventKey c Down _ _) gstate
    | c == SpecialKey KeyUp     = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate North
    | c == SpecialKey KeyLeft   = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate West
    | c == SpecialKey KeyDown   = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate South
    | c == SpecialKey KeyRight  = setPlayerState Walking $ setKeyState Down $ modPlayer gstate $ changePlayerDir gstate East
    | c == SpecialKey KeyEsc    = gstate { currentState = Paused }
    | c == Char ','             = modBombs gstate $ addBombs (getGridPos $ player gstate)

inputKey (EventKey (SpecialKey _) Up _ _) gstate = setPlayerState Idle $ gstate {keyState = Up}
inputKey _ gstate = gstate

handleInput :: Event -> GameState -> GameState
handleInput ev gs
                | currentState gs == Running && playerState /= Dying = inputKey ev gs
                | currentState gs == Paused                          = inputKeyPaused ev gs
                | currentState gs == GameOver                        = inputKeyMenu ev gs
                | currentState gs == Victory                         = inputKeyVictory ev gs
                | otherwise                                          = gs
                    where playerState = state $ player gs
inputKeyPaused :: Event -> GameState -> GameState
inputKeyPaused (EventKey c Down _ _) gstate
                | c == SpecialKey KeyEsc        = gstate { currentState = Running }
                | c == SpecialKey KeySpace      = error "Quit the game"
inputKeyPaused _ gstate = gstate

inputKeyMenu :: Event -> GameState -> GameState
inputKeyMenu (EventKey c Down _ _) gstate
                | c == Char 'y'       = initGame
                | c == Char 'n'       = error "Much noob very wow"
inputKeyMenu _ gstate = gstate

inputKeyVictory :: Event -> GameState -> GameState
inputKeyVictory (EventKey c Down _ _) gstate
                | c == SpecialKey KeyEsc       = undefined
                | c == SpecialKey KeySpace     = initGame
inputKeyVictory _ gstate = gstate

setKeyState :: KeyState -> GameState -> GameState
setKeyState k gstate = gstate { keyState = k}

setPlayerState :: PlayerState -> GameState -> GameState
setPlayerState pState gstate = gstate { player = p { state = pState } }
                                where p = player gstate

checkIfPlayerIsAlive :: GameState -> GameState
checkIfPlayerIsAlive gs | health (player gs) == Alive  = gs
                        | otherwise                    = gs { currentState = GameOver }

checkPlayerVictory :: GameState -> GameState
checkPlayerVictory gs | allDead     = gs { currentState = Victory }
                      | otherwise   = gs
            where allDead = all ( == Dead) (map health (enemies gs))

checkIfEnemiesLeft :: GameState -> GameState
checkIfEnemiesLeft gs | length (filter ( == Alive ) (map health (enemies gs ))) > 0  = gs
                    | otherwise                         = gs { currentState = GameOver}

