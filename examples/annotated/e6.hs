data T a where {TInt :: (a ~ Int) => a -> T a; TBool :: (a ~ Bool) => a -> T a}
e = let a :: (T t27,t27,t27->t32)->t32 = \z -> case z of {(TInt n, y, f) -> f n; (TBool b, y, f) -> f y} in (a)

--a :: (T t27,t27,t27->t32)->t32
--a z = case z of {(TInt n, y, f) -> f n; (TBool b, y, f) -> f y}
-- ghc accepts
