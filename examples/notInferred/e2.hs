data T a where {TInt :: (a ~ Int) => a -> T a; TAny :: (a ~ Bool) => a -> T a}
e2 = \x -> case x of {(TInt n,TAny b) -> (n > 0);(TAny b,TInt n) -> b}
-- This is just very wrong.
