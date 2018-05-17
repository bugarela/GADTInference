data T a where {T1 :: (a ~ Int) => a -> T a; T3 :: (a ~ Bool) => a -> T a}
e = let a :: (T t27,t27,t27->t32)->t32 = \z -> case z of {(T1 n, y, f) -> f n; (T3 b, y, f) -> f y} in (a)

--a :: (T t27,t27,t27->t32)->t32
--a z = case z of {(T1 n, y, f) -> f n; (T3 b, y, f) -> f y}
-- ghc accepts
