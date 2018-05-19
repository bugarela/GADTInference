data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e = let f = (\x -> case x of {TInt n -> n; TBool b -> b}) in \z -> if (f (TBool z)) then (f (TInt 1)) else (f (TInt 2))
-- ghc does not infer
