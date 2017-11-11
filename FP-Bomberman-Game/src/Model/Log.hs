module Model.Log where

import Model.GameState

readCurrentHighscore :: IO String
readCurrentHighscore = do
                  content <- readFile "res/score.txt"
                  return content

printCollision :: GameState -> String
printCollision gs = show $ checkCollisionField (player gs) (grid gs)

writeNewHighScore :: Int -> IO ()
writeNewHighScore score =
                     do
                        oldScore <- readCurrentHighscore
                        let intOldScore = read oldScore :: Int
                        if score > intOldScore
                        then writeFile "res/score.txt" $ show score
                        else return ()

