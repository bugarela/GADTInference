data T a where {TInt :: (a ~ Bool) => Int -> T a; TAny :: T a}
e = let f = (\x -> case x of {TAny -> True;TInt n -> (n > 0)}) in \z -> if (f z) then 1 else 2
-- T t -> Int
