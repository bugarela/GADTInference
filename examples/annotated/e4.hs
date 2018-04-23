data T a where {T1 :: (Bool ~ a) => Int -> T a}
e2 = let g :: T Bool -> Bool = \x -> case x of { T1 n -> n > 0 } in g

--g :: T Bool -> Bool
--g x = case x of { T1 n -> n > 0 }
