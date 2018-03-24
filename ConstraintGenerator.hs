module ConstraintGenerator where

import Head
import Type
import Data.List

tiContext g i = if l /= [] then (freshInstance t) else error ("Variable " ++ i ++ " undefined\n")
    where
        l = dropWhile (\(i' :>: _) -> i /= i' ) g
        (_ :>: t) = head l

conGen :: [Assump] -> Expr -> TI (SimpleType,Constraint)

--VAR
conGen g (Var x) = do t <- tiContext g x
                      return (t, Simp E)

--CON
conGen g (Con c) = do t <- tiContext g c
                      return (t, Simp E)

--LIT
conGen g (Lit a) = return (TLit a, Simp E)

--APP
conGen g (App e1 e2) = do (t1,f1) <- conGen g e1
                          (t2,f2) <- conGen g e2
                          a <- freshVar
                          let f = Conj ([f1] ++ [f2] ++ [(t1 ~~ (t2 --> a))])
                          return (a,f)

--ABS
conGen g (Lam i e) = do a <- freshVar
                        (t,f) <- conGen (g /+/ [i:>:(Forall a)]) e
                        return (a --> t ,f)

--LET
conGen g (Let (i,e1) e2) = do a <- freshVar
                              (t,f1) <- conGen (g /+/ [i:>:(Forall a)]) e1
                              let fs = Conj ([f1] ++ [a ~~ t])
                              let s = sSolve (simple fs)
                              let t' = apply s t
                              let q = quantify (tv t' \\ tv (apply s g)) t'
                              (vs,q') <- (freshSubst q)
                              let bs = tv vs
                              (v,f2) <- conGen (g /+/ [i:>:(Forall (inst vs q'))]) e2
                              let f = Conj ([f2] ++ [(Impl (map makeTvar (tv g)) bs E fs)])
                              return (v,f)

--LETA
conGen g (LetA (i,(Forall t),e1) e2) = do (t',f1) <- conGen (g /+/ [i:>:(quantify (tv t) t)]) e1
                                          (v,f2) <- conGen (g /+/ [i:>:(quantify (tv t) t)]) e2
                                          let f = Conj ([f2] ++ [(Impl (map makeTvar (tv g)) (tv t) E f1)] ++ [t ~~ t'])
                                          return (v,f)

--CASE
conGen g (Case e ls) = do (te,fe) <- conGen g e
                          a <- freshVar
                          fs <- mapM (conGenAlt g a te) ls
                          let f = Conj ([fe] ++ fs)
                          return (a,f)

conGenAlt g a te (p,e) = do (TArr ti vi,fi) <- conGenPat g p e
                            return (Conj ([fi] ++ [te ~~ ti] ++ [a ~~ vi]))

conGenPat g (PVar i) e = do b <- freshVar
                            (te,fe) <- conGen (g /+/ [i:>:(Forall b)]) e
                            return (b --> te,fe)
conGenPat g (PLit tipo) e = do (te,fe) <- conGen g e
                               return (TLit tipo --> te, fe)
conGenPat g (PCon i []) e = do t <- tiContext g i
                               (te,fe) <- conGen g e
                               return (t --> te, fe)
conGenPat g (PCon i xs) e = do t <- tiContext g i
                               let ps = conParameters t
                               let c = ret t
                               let phi = zipWith (:>:) xs ps
                               (te,fe) <- conGen (g /+/ phi) e
                               return (c --> te,fe)
