data T a where {T1 :: (a ~ Int) => a -> T a; T2 :: a -> T a; T3 :: (a ~ Bool) => a -> T a}
e = let f = (\x -> case x of {T1 n -> n; T3 b -> b}) in \z -> if (f (T3 z)) then (f (T1 1)) else (f (T1 2))
-- ghc does not infer
