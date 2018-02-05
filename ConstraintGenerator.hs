module ConstraintGenerator where

import Head
import Type

tiContext g i = if l /= [] then (freshInstance t) else error ("Variable " ++ i ++ " undefined\n")
    where
        l = dropWhile (\(i' :>: _) -> i /= i' ) g
        (_ :>: t) = head l

--VAR
conGen g (Var x) = do t <- tiContext g x
                      return (t, [])

--CON
conGen g (Con c) = undefined

--APP
conGen g (App e1 e2) = do (t1,f1) <- conGen g e1
                          (t2,f2) <- conGen g e2
                          a <- freshVar
                          let f = f1 ++ f2 ++ [(t1 ~~ (t2 --> a))]
                          return (a,f)

--ABS
conGen g (Lam i e) = do a <- freshVar
                        (t,f) <- conGen (g /+/ [i:>:(Forall a)]) e
                        return (a --> t ,f)

-- UNFINISHED RULES:
--LET
conGen g (Let (i,e1) e2) = do a <- freshVar
                              (t,f1) <- conGen (g /+/ [i:>:(Forall a)]) e1
                              let fs = f1 ++ [a ~~ t]
                              let s = unifyConstraints (simple fs)
                              let t' = apply s t
                              let q = quantify (tv t) t
                              (v,f2) <- conGen (g /+/ [i:>:q]) e2
                              let f = f2 ++ [(Impl (tv g) q E (Conj fs))]
                              return (v,f)

--CASE
conGen g (Case e ls) = do (te,fe) <- conGen g e
                          a <- freshVar
                          fs <- mapM (conGenAlt g a te) ls
                          let f = fe ++ (foldr1 (++) fs)
                          return (a,f)

conGenAlt g a te (p,e) = do (t,ft) <- conGenPat g p
                            (v,fv) <- conGen g e
                            return (ft ++ fv ++ [te ~~ t] ++ [a ~~ v])

conGenPat g p = undefined
