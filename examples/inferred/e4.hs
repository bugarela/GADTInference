data T a where {T1 :: (a ~ Bool) => Int -> T a; T2 :: T a}
e = \x -> case x of {T2 -> True;T1 n -> (n > 0)}
-- T t -> Bool
