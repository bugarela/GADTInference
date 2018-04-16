data T a where {T1 :: (Bool ~ a) => Int -> T a}
e3 = let g :: (T a) -> Bool = (\x -> case x of { T1 n -> n > 0 }) in g
