data T a where {T1 :: (a ~ Int) => a -> T a; T2 :: a -> T a; T3 :: (a ~ Bool) => a -> T a}
e = let f = (\z -> case z of {(T1 n, y) -> if y then n else n; (T2 x, y) -> if y then x else x; (T3 b, y) -> y}) in f
-- (T t, Bool) -> t
