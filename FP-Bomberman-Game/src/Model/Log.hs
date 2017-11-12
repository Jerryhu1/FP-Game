module Model.Log where

import Model.GameState
import Control.Monad (when)

readCurrentHighscore :: IO String
readCurrentHighscore = readFile "res/score.txt"


printCollision :: GameState -> String
printCollision gs = show $ checkCollisionSurr (player gs) (grid gs)

writeNewHighScore :: Int -> IO ()
writeNewHighScore score =
                     do
                        oldScore <- readCurrentHighscore
                        let intOldScore = read oldScore :: Int
                        when (score > intOldScore)$ writeFile "res/score.txt" $ show score

