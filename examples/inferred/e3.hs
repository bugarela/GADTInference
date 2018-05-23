data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e = let f = (\x -> \y -> case (x,y) of {(TInt x, TInt y) -> x;(TInt x, TBool y) -> y;(TBool x, TInt y) -> y;(TBool x, TBool y) -> x}) in f

--f :: T a -> T b -> b
--f = (\x -> \y -> case (x,y) of {(TInt x, TInt y) -> x;(TInt x, TBool y) -> y;(TBool x, TInt y) -> y;(TBool x, TBool y) -> x})
