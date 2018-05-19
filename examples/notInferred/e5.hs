data T a where {TInt :: (Bool ~ a) => Int -> T a}
e3 = \x -> case x of { TInt n -> n > 0 }
-- see annotated/e1.hs -- ghc does not infer
