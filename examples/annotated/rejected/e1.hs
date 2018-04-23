data T a where {T1 :: (Bool ~ a) => Int -> T a}
e3 = let g :: Bool -> Bool = \x -> case x of {True -> 0} in g
