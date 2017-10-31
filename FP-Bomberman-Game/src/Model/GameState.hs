module Model.GameState where

    import System.Random

    import Model.Player
    import Model.Grid
    import Model.Typeclasses.Positioned

    data GameState = GameState {
        player       :: Player,
        grid         :: Grid,
        currentState :: CurrentState,
        gen          :: StdGen
        -- explosions :: [Field]
        -- enemies :: [Player]
    }

    data CurrentState = Loading | Running | Paused | GameOver
            deriving(Show, Eq)

    initGame :: GameState
    initGame = GameState initPlayer createGrid Loading (mkStdGen 0)

    getRNumber :: IO Int
    getRNumber = getStdRandom (randomR(1,100))
     
    {-
        Generates random blocks by RNG in Gamestate
        Chance is now set at 60%, should be dynamic
        Needs a limit of amount of blocks?
    -}
    setBreakableBlocks :: GameState -> IO GameState
    setBreakableBlocks gs = 
            do 
                g <- return $ grid gs
                newGrid <- mapM setBreakableBlock g
                putStrLn(show newGrid)
                return gs { grid = newGrid, currentState = Running}
    
    setBreakableBlock :: Field -> IO Field
    setBreakableBlock f = do
                            rng <- getRNumber
                            let obj = gameObject f
                            return (case obj of 
                                Empty | rng > 40      -> f { gameObject = StoneBlock}
                                      | otherwise     -> f
                                _                     -> f )
   

    checkIfPlayerCollision :: Player -> Grid -> Bool
    checkIfPlayerCollision p (x:[]) | gameObject x == Empty = False
                                    | gameObject x == PowerUp = False -- Should still check for collision and pick up item
                                    | otherwise = checkField p x
    checkIfPlayerCollision p (x:xs) | obj == Empty = checkIfPlayerCollision p xs
                                    | otherwise    = if (checkField p x == True) then True else checkIfPlayerCollision p xs
                            where obj = gameObject x

    testField :: Field
    testField = Field (-325, -325) MetalBlock

    testPlayer :: Player
    testPlayer = (Player "Jerry" 100 (-325,-275) 10 North "test")

    checkField :: Player -> Field -> Bool
    checkField pl f = case playerDirection pl of
                      North | (getY pl == getY f + 50 && (getX pl >= getX f && getX pl < getX f + 49)
                              || (getX pl + 49 >= getX f && getX pl + 49 < getX f + 49)) -> True
                            | otherwise -> False
                      East  | (getX pl + 50) == getX f && (getY pl >= getY f - 49)
                              && (getY pl - 50 <= getY f - 1 ) -> True
                            | otherwise -> False
                      South |  (getY pl - 50 == getY f && (getX pl >= getX f && getX pl < getX f + 49)
                                 || (getX pl + 49 >= getX f && getX pl + 49 < getX f + 49)) -> True
                            | otherwise -> False
                      West  |(getX pl) == getX f + 50 && (getY pl >= getY f - 49)
                               && (getY pl - 50 <= getY f - 1 ) -> True
                            | otherwise -> False
