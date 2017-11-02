module Model.Typeclasses.Positioned where
    
 class Renderizable a where
    render :: a -> Picture

