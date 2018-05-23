data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e = \x -> case x of {TInt n -> n; TBool b -> b}
-- ghc does not infer
