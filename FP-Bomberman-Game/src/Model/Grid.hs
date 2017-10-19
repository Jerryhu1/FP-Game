module Model.Grid where

    import Model.Typeclasses.Positioned

    data Field = Field {
        fieldPosition :: (Int,Int),
        gameObject :: GameObject
    }

    type Grid = [Field]

    data Block  = Block {
    }

    data GameObject = PowerUp | MetalBlock | StoneBlock | Empty
    -- Misschien PowerUp onderdeel maken van Metalblock?

    instance Positioned Field where
         pos f = fieldPosition f