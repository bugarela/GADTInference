data Erk a b where {I :: (a ~ Int) => Int -> Erk a b; B :: (b ~ Bool) => Bool -> Erk a b}
e = \x -> case x of {I a -> a + 1; B b -> b && True}
-- Example 2 SCP-GADT.pdf (pg 10)
