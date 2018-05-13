data T a where {T1 :: (a ~ Bool) => Int -> T a; T2 :: T a}
e4 = \x -> case x of {T2 -> True;T1 n -> (n > 0)}
