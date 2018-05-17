data T a where {T1 :: (a ~ Bool) => Int -> T a; T2 :: T a}
e = let f = (\x -> case x of {T2 -> True;T1 n -> (n > 0)}) in \z -> if (f z) then 1 else 2
-- T t -> Int
