data T a where {T1 :: (a ~ Bool) => b -> T a; T2 :: T a}
e = let f = (\x -> case x of {T1 n -> (case n of {T1 y -> y > 1});T2 -> True}) in f
-- b is a rigid type variable. Maybe this is wrong.
