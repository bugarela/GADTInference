data T a where {T1 :: (a ~ Int) => a -> T a; T2 :: a -> T a; T3 :: (a ~ Bool) => a -> T a}
e2 = let g :: (T a, b) -> a = \z -> case z of {(T1 n, _) -> n; (T3 b, _) -> b} in g
