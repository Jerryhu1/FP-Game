module Model.Log where


printCollision :: GameState -> String
printCollision gs = show $ checkCollisionField (player gs) (grid gs)