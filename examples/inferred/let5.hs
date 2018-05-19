data T a where {TInt :: (Bool ~ a) => Int -> T a}
e = let f = (\x -> case x of { TInt n -> n > 0 }) in f
-- see annotated/e1.hs -- ghc does not infer
