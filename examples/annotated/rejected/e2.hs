data R a where {R1 :: (a ~ Int) => Int -> R a; R2 :: a -> R a}
e6 = let testR :: (R b,c) -> b = \z -> case z of {(R1 x,y) -> if y then x else (x + 1); (R2 x,y) -> if (x===y) then x else y} in testR

-- Example 3 SCP-GADT.pdf (pg 10)

--(===) :: a -> a -> Bool
--(===) a b = True
--testR :: (R b, c) -> b
--testR z = case z of {(R1 x,y) -> if y then x else (x + 1); (R2 x,y) -> if (x===y) then x else y}
