data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e1 = \z -> case z of {(TInt n, y) -> n; (TBool b, y) -> y}
-- see annotated/e3.hs -- ghc does not infer
