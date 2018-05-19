data T a where {TInt :: (a ~ Bool) => Int -> T a; TAny :: T a}
e = \x -> case x of {TAny -> True;TInt n -> (n > 0)}
-- T t -> Bool
