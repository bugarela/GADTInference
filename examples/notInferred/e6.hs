data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a}
e = \z -> case z of {(TInt n, y, f) -> f n; (TBool b, y, f) -> f y}
-- ghc does not infer
-- see annotated/e6.hs
