data T a where {T1 :: (Bool ~ a) => Int -> T a}
e1 = let g :: T Bool -> Bool = \x -> case x of { T1 n -> n > 0 } in g
-- should reject
