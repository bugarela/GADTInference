data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e = let f = (\z -> case z of {(TInt x, y) -> if y then x else (x + 1); (TAny x, y) -> if (x===y) then x else y}) in f
-- Example 3 SCP-GADT.pdf (pg 10)
