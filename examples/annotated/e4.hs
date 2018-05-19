data T a where {TInt :: (Bool ~ a) => Int -> T a}
e2 = let g :: T Bool -> Bool = \x -> case x of { TInt n -> n > 0 } in g

--g :: T Bool -> Bool
--g x = case x of { TInt n -> n > 0 }
