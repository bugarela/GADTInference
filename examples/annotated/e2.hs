data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e2 = let g :: (T a, b) -> a = \z -> case z of {(TInt n, _) -> n; (TBool b, _) -> b} in g
