data T a where {TInt :: (a ~ Bool) => b -> T a; TAny :: T a}
e = let f = (\x -> case x of {TInt n -> (case n of {TInt y -> y > 1});TAny -> True}) in f
-- b is a rigid type variable. Maybe this is wrong.
