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
        do putStrLn $ (printCollision gstate ++ show (player gstate))
           return gstate


  printCollision :: GameState -> String
  printCollision gs = show $ checkIfPlayerCollision (player gs) (grid gs)

  -- | Handle user input
  input :: Event -> GameState -> IO GameState
  input e gstate = return (inputKey e gstate)
  
  -- Eerste opzet lopende player
  inputKey :: Event -> GameState -> GameState
  inputKey (EventKey c Down _ _) gstate
    | c== SpecialKey KeyUp     = modPlayer gstate $ movePlayer (0,1)
    | c== SpecialKey KeyLeft   = modPlayer gstate $ movePlayer (-1,0)
    | c== SpecialKey KeyDown   = modPlayer gstate $ movePlayer (0,-1)
    | c== SpecialKey KeyRight  = modPlayer gstate $ movePlayer (1,0)
    | c== Char ','             = modGrid gstate $ addGameObject $ Field {fieldPosition = getGridPos $player gstate, gameObject = Bomb}
    | c== Char '.'             = modGrid gstate $ addGameObject $ Field {fieldPosition = getGridPos $player gstate, gameObject = PowerUp}  
  inputKey _ gstate = gstate 
  
  movePlayer :: Pos -> Player -> Player
  movePlayer pos player' = setPos (addVel delta $ getPos player') player'
    where delta = (*.) pos (* (velocity player'))
  

  addVel :: Pos -> Pos -> Pos
  addVel (x,y) (x',y') = (newX, newY)
    where newX = max (-375) $ min 375 $ x+x'
          newY = max (-375) $ min 375 $ y+y'
  
  addGameObject :: Field -> Grid -> Grid
  addGameObject newField [] = []
  addGameObject newField (x:xs) | fieldPosition newField == fieldPosition x   = newField : addGameObject newField xs
                                | otherwise                                   = x : addGameObject newField xs
  
  
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
  
  
  
  
  
  
