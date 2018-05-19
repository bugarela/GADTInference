data T a where {TInt :: (a ~ Bool) => Int -> T a; TAny :: T a}
e2 = let f = (\x -> case x of {TInt n -> (n > 0);TAny -> True}) in f
-- T t -> Bool
