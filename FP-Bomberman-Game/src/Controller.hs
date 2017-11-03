-- | This module defines how the state changes
--   in response to time and user input
module Controller where
  
  import Model.GameState
  import Model.Player
  import Model.Typeclasses.Positioned
  import Model.Grid
  import Model.GameObject
  
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import System.Random
  
  -- | Handle one iteration of the game
  step :: Float -> GameState -> IO GameState
  step secs gstate =
        do 
           let gs | currentState gstate == Loading  = setBreakableBlocks gstate 
                  | keyState gstate == Down         = return $ modPlayer gstate $ checkifMovePlayer gstate
                  | otherwise                       = return $ modBombs gstate setTimer
           g <- gs
           putStrLn $ (printCollision gstate ++ show (player gstate) ++ show secs)
           return g

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
  

  

  --change the direction in which the player is positioned
  changePlayerDir :: GameState -> Direction -> Player -> Player
  changePlayerDir gstate dir player' = checkifMovePlayer gstate $ setDir dir player'
  
  checkifMovePlayer :: GameState -> Player -> Player
  checkifMovePlayer gs p  | checkCollisionField p $ grid gs     = checkCollisionBombs p $ bombs gs 
                          | otherwise                           = movePlayerInDir p                      


  --TO DO: SAMENVOEGEN--
  checkCollisionBombs :: Player -> Bombs -> Player
  checkCollisionBombs p []     = p
  checkCollisionBombs p (x:xs)  | checkCollision p x            = playerCollisionBomb (bombStatus x) p
                                | otherwise                     = checkCollisionBombs p xs

  checkCollisionField :: Player -> Grid -> Bool
  checkCollisionField _ []     = False
  checkCollisionField p (x:xs)  | gameObject x /= Empty &&  gameObject x /= PowerUp && checkCollision p x  = True
                                | otherwise                 = checkCollisionField p xs
                        
 
  modBombs :: GameState -> (Bombs-> Bombs) -> GameState
  modBombs gstate f = gstate {bombs = f $ bombs gstate}
  
  modGrid :: GameState -> (Grid -> Grid) -> GameState
  modGrid gstate f = gstate { grid = f $ grid gstate, keyState = Down}
  
  modPlayer :: GameState -> (Player -> Player) -> GameState
  modPlayer gstate f = gstate { player = f $ player gstate, keyState = Down}
  

  
  
  
  
  
  
  
  
  
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
  
  
  
  
  
  
