data T a where {T1 :: (Bool ~ a) => Int -> T a}
e = let f = (\x -> case x of { T1 n -> n > 0 }) in f
-- see annotated/e1.hs -- ghc does not infer
