data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e = let f = (\z -> case z of {(TInt n, y) -> n; (TBool b, y) -> y}) in f
-- see annotated/e3.hs -- ghc does not infer -- does not have principal type
