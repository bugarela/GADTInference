data P a where {P1 :: (a ~ Bool) => b -> P a; P2 :: P a}
e = let f = (\x -> case x of {P1 n -> (case n of {P1 y -> y > 1});P2 -> True}) in f
-- b is a rigid type variable. Maybe this is wrong.
