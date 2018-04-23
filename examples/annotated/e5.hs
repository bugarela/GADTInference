data Erk a b where {I :: (a ~ Int) => Int -> Erk a b; B :: (b ~ Bool) => Bool -> Erk a b}
e5 = let f :: Erk a a -> a = \x -> case x of {I a -> a + 1; B b -> b && True} in f

-- Example 2 SCP-GADT.pdf (pg 10)
