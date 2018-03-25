module ConstraintGenerator where

import Head
import Type
import ConstraintSolver
import Data.List

tiContext (_,g) i = if l /= [] then (freshInstance t) else error ("Variable " ++ i ++ " undefined\n")
    where
        l = dropWhile (\(i' :>: _) -> i /= i' ) g
        (_ :>: t) = head l

gadtContext (g,_) i = if l /= [] then freshInstC t c  else error ("Constructor " ++ i ++ " undefined\n")
    where
        l = dropWhile (\(i',_,_) -> i /= i' ) g
        (_,t,c) = head l


conGen :: ([GADT],[Assump]) -> Expr -> TI (SimpleType,Constraint)

--VAR
conGen g (Var x) = do t <- tiContext g x
                      return (t, Simp E)

--CON
conGen g (Con d) = do (t,c) <- gadtContext g d
                      return (t,Simp c)

--LIT
conGen g (Lit a) = return (TLit a, Simp E)

--APP
conGen g (App e1 e2) = do (t1,f1) <- conGen g e1
                          (t2,f2) <- conGen g e2
                          a <- freshVar
                          let f = Conj ([f1] ++ [f2] ++ [(t1 ~~ (t2 --> a))])
                          return (a,f)

--ABS
conGen (d,g) (Lam i e) = do a <- freshVar
                            (t,f) <- conGen (d,(g /+/ [i:>:(Forall a)])) e
                            return (a --> t ,f)

--LET
conGen (d,g) (Let (i,e1) e2) = do a <- freshVar
                                  (t,f1) <- conGen (d,(g /+/ [i:>:(Forall a)])) e1
                                  let fs = Conj ([f1] ++ [a ~~ t])
                                  let s = sSolve (simple fs)
                                  let t' = apply s t
                                  let q = quantify (tv t' \\ tv (apply s g)) t'
                                  (vs,q') <- (freshSubst q)
                                  let bs = tv vs
                                  (v,f2) <- conGen (d,(g /+/ [i:>:(Forall (inst vs q'))])) e2
                                  let f = Conj ([f2] ++ [(Impl (map makeTvar (tv g)) bs E (instF vs fs))])
                                  return (v,f)

--LETA
conGen (d,g) (LetA (i,(Forall t),e1) e2) = do (t',f1) <- conGen (d,(g /+/ [i:>:(quantify (tv t) t)])) e1
                                              (v,f2) <- conGen (d,(g /+/ [i:>:(quantify (tv t) t)])) e2
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

conGenPat (d,g) (PVar i) e = do b <- freshVar
                                (te,fe) <- conGen (d,(g /+/ [i:>:(Forall b)])) e
                                return (b --> te,fe)
conGenPat g (PLit tipo) e = do (te,fe) <- conGen g e
                               return (TLit tipo --> te, fe)
conGenPat g (PCon i []) e = do (t,c') <- gadtContext g i
                               (te,fe) <- conGen g e
                               return (t --> te, fe)
conGenPat (d,g) (PCon i xs) e = do (t,c) <- gadtContext (d,g) i
                                   let ps = conParameters t
                                   let r = ret t
                                   let k = cons r
                                   let as = findAs r
                                   phi <- (freshs as)
                                   let g' = zipWith (:>:) xs (map toType (apply phi ps))
                                   (te,fe) <- conGen (d,(g /+/ g')) e
                                   let bs = findBs ps as
                                   let as' = map snd phi
                                   let f = Impl (as' ++ map makeTvar (tv g ++ tv te)) bs c fe
                                   return (foldl1 TApp ([TCon k]++as') --> te,f)
