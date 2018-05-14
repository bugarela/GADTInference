data T a where {T1 :: (a ~ Int) => a -> T a; T2 :: a -> T a; T3 :: (a ~ Bool) => a -> T a}
e1 = \z -> case z of {(T1 n, y) -> n; (T3 b, y) -> y}
-- see annotated/e3.hs
