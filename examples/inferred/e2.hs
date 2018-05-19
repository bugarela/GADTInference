data T a where {TInt :: (a ~ Bool) => Int -> T a; TAny :: T a}
e = \x -> case x of {TInt n -> (n > 0);TAny -> True}
-- T t -> Bool
