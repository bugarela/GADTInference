data T a where {T1 :: (a ~ Bool) => Int -> T a; T2 :: T a}
e2 = let f = (\x -> case x of {T1 n -> (n > 0);T2 -> True}) in f
