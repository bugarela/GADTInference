data T a where {T1 :: (a ~ Int) => a -> T a; T2 :: (a ~ Bool) => a -> T a}
e2 = \x -> case x of {(T1 n,T2 b) -> (n > 0);(T2 b,T1 n) -> b}
