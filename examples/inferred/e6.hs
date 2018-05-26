--(===):: a -> a -> Bool
--_ === _ = True

data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e = \x -> case x of {(TAny x, TAny y) -> if (x===y) then x else y; (TAny x, TInt y) -> y}
-- (T a, a) -> a
