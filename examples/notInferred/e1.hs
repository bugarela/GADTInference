data T a where {T1 :: (Bool ~ a) => Int -> T a}
e3 = \x -> case x of { T1 n -> n > 0 }
-- see annotated/e1.hs
