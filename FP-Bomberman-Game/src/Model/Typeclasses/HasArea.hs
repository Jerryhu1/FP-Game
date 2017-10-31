module Model.Typeclasses.HasArea where
    import Model.Typeclasses.Positioned
    
    class Positioned a => HasArea a where
        inArea :: a -> (Int,Int) -> Bool

