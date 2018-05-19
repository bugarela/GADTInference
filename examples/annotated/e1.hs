data T a where {TInt :: (Bool ~ a) => Int -> T a}
e3 = let g :: T a -> Bool = \x -> case x of { TInt n -> n > 0 } in g
