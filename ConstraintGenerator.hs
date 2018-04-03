module ConstraintGenerator where

import Head
import Type
import ConstraintSolver
import Data.List

conGen :: ([Assump]) -> Expr -> TI (SimpleType,Constraint)

--VAR
conGen g (Var x) = do (t,c) <- tiContext g x
                      return (t, Simp c)

--CON
conGen g (Con i) = do (t,c) <- tiContext g i
                      return (t, Simp c)

--LIT
conGen g (Lit a) = return (TLit a, Simp E)

-- IF
conGen g (If x t e) = do (tx,fx) <- conGen g x
                         (tt,ft) <- conGen g t
                         (te,fe) <- conGen g e
                         let s = unify tx (TLit Bool)
                         let u = unify (apply s tt) (apply s te)
                         let s' = s @@ u
                         return (apply s' tt, apply s' (Conj ([fx] ++ [ft] ++ [fe])))

--APP
conGen g (App e1 e2) = do (t1,f1) <- conGen g e1
                          (t2,f2) <- conGen g e2
                          a <- freshVar
                          let f = Conj ([f1] ++ [f2] ++ [(t1 ~~ (t2 --> a))])
                          return (a,f)

--ABS
conGen g (Lam i e) = do a <- freshVar
                        (t,f) <- conGen (g /+/ [i:>:(convert a)]) e
                        return (a --> t ,f)

--LET
conGen g (Let (i,e1) e2) = do a <- freshVar
                              (t,f1) <- conGen (g /+/ [i:>:(convert a)]) e1
                              let fs = Conj ([f1] ++ [a ~~ t])
                              let s = sSolve (simple fs)
                              let t' = apply s t
                              let q = quantify (tv t' \\ tv (apply s g)) t'
                              (vs,q') <- (freshSubstC q)
                              let bs = tv vs
                              (v,f2) <- conGen (g /+/ [i:>:(convert (inst vs q'))]) e2
                              let f = Conj ([f2] ++ [(Impl (map makeTvar (tv g)) bs E (instF vs fs))])
                              return (v,f)

--LETA
conGen g (LetA (i,(Constrained (Forall t) cs),e1) e2) = do (t',f1) <- conGen (g /+/ [i:>:(quantifyAll t)]) e1
                                                           (v,f2) <- conGen (g /+/ [i:>:(quantifyAll t)]) e2
                                                           let f = Conj ([f2] ++ [(Impl (map makeTvar (tv g)) (tv t) E f1)] ++ [t ~~ t'])
                                                           return (v,f)

--CASE
conGen g (Case e ls) = do (te,fe) <- conGen g e
                          a <- freshVar
                          fs <- mapM (conGenAlt g a te) ls
                          let f = Conj ([fe] ++ fs)
                          return (a,f)

conGenAlt g a te (p,e) = do (ti, fi, g', _) <- conGenPat g p e
                            (te',fe) <- conGen g' e
                            return (Conj ([fi] ++ [fe] ++ [te ~~ ti] ++ [a ~~ te']))


conGenPats g [] = return ([],[],[],[])
conGenPats g (p:ps) = do (t1,f1,g1,b1) <- conGenPat' g p
                         (t2,f2,g2,b2) <- conGenPats g1 ps
                         return ([t1] ++ t2, [f1] ++ f2, g /+/ g1 /+/ g2, b1 ++ b2)

conGenPat' g (PVar i) = do b <- freshVar
                           return (b, Simp E, (g /+/ [i:>:(convert b)]), [b])
conGenPat' g (PLit tipo) = do return (TLit tipo, Simp E, g, [])
conGenPat' g (PCon i []) = do (t,c') <- tiContext g i
                              return (t, Simp E, g, [])
conGenPat' g (PCon i xs) = do (t,c) <- tiContext g i
                              (ts,fs,gp,as) <- conGenPats g xs
                              b <- freshVar
                              let ta = foldr1 TArr (ts ++ [b])
                              let u = unify t ta
                              let g' = apply u gp
                              return (apply u b, apply u (Conj fs), g',apply u as)

conGenPat g (PCon i xs) e = do (t,c) <- tiContext g i
                               (ts,fs,gp,as) <- conGenPats g xs
                               b <- freshVar
                               let ta = foldr1 TArr (ts ++ [b])
                               let u = unify t ta
                               let g' = apply u gp
                               (te,fe) <- conGen g' e
                               let bs = findBs ta (map idOf (as ++ [b]))
                               let f = Impl (as ++ map makeTvar (tv g' ++ tv te)) bs c fe
                               let f' = apply u (Conj ([f] ++ fs))
                               return (apply u b, f', g',apply u as)
conGenPat g p _ = conGenPat' g p
