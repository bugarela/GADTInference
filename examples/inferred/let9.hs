data T a where {T1 :: (a ~ Int) => a -> T a; T2 :: a -> T a; T3 :: (a ~ Bool) => a -> T a}
e = let f = (\x -> case x of {T1 n -> n; T3 b -> b}) in f
-- ghc does not infer
