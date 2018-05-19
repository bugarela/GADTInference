--(===):: a -> a -> Bool
--_ === _ = True
data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a; TAny :: a -> T a}
e = \x -> case x of {(TInt n, _) -> n; (TAny x, y) -> if (x===y) then x else y; (TBool b, _) -> b}
-- (T t, t) -> t
