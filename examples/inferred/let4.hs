data T a where {TInt :: (a ~ Bool) => Int -> T a; TAny :: T a}
e = \x -> case (TInt x) of {TInt n -> (n > 0);TAny -> True}
-- Int -> Bool
