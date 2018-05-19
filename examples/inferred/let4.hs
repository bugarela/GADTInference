data T a where {T1 :: (a ~ Bool) => Int -> T a; T2 :: T a}
e = \x -> case (T1 x) of {T1 n -> (n > 0);T2 -> True}
-- Int -> Bool
