data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e = let f = (\z -> case z of {(TInt x, y) -> (case y of {TBool _ -> True; TInt _ -> 1});(TBool x, y) -> (case y of {TBool _ -> 1; TInt _ -> True})}) in f
