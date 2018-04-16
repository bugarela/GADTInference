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

-- IF
conGen g (If x t e) = do (tx,fx) <- conGen g x
                         (tt,ft) <- conGen g t
                         (te,fe) <- conGen g e
                         return (te, (Conj ([tx ~~ (TCon "Bool")] ++ [tt ~~ te] ++ [fx] ++ [ft] ++ [fe])))

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
conGen g (LetA (i,(Constrained t cs),e1) e2) = do t1 <- freshInstance t
                                                  (t',f1) <- conGen (g /+/ [i:>:(Constrained t cs)]) e1
                                                  (v,f2) <- conGen (g /+/ [i:>:(Constrained t cs)]) e2
                                                  let f = Conj ([f2] ++ [(Impl (map makeTvar (tv g)) (tv t) E f1)] ++ [t1 ~~ t'])
                                                  return (v,f)

--CASE
conGen g (Case e ls) = do (te,fe) <- conGen g e
                          a <- freshVar
                          fs <- mapM (conGenAlt g a te) ls
                          let f = Conj ([fe] ++ fs)
                          return (a,f)

conGenAlt g a te (p,e) = do (t, fi) <- conGenPat g a p e
                            let ti = leftArr t
                            let vi = rightArr t
                            return (Conj ([fi] ++ [te ~~ ti]))


conGenPats g [] = return ([],[],g,[])
conGenPats g (p:ps) = do (t1,c1,g1,b1) <- conGenPat' g p
                         (t2,c2,g2,b2) <- conGenPats g1 ps
                         return ([t1] ++ t2, [c1] ++ c2, (g /+/ g1) /+/ g2, b1 ++ b2)

conGenPat' g (PVar i) = do b <- freshVar
                           return (b, E, (g /+/ [i:>:(convert b)]), [b])
conGenPat' g (PCon i []) = do (t,c') <- tiContext g i
                              return (t, E, g, [])
conGenPat' g (PCon i xs) = do (t,c) <- tiContext g i
                              (ts,cs,gp,as) <- conGenPats g xs
                              b <- freshVar
                              let ta = foldr1 TArr (ts ++ [b])
                              let u = unify t ta
                              let as' = intersect as (map makeTvar (tv (rightArr t)))
                              let g' = apply u gp
                              return (apply u b, apply u (SConj ([c] ++ cs)), g',as)

conGenPat g a (PCon i xs) e = do (t,c) <- tiContext g i
                                 (ts,cs,gp,as) <- conGenPats g xs
                                 b <- freshVar
                                 let ta = foldr1 TArr (ts ++ [b])
                                 let u = unify t ta
                                 let g' = apply u gp
                                 (te,fe) <- conGen g' e
                                 let bs = findBs ta (map idOf (as ++ [b]))
                                 let as' = intersect as (map makeTvar (tv (rightArr t)))
                                 let f = Impl ([a] ++ as' ++ map makeTvar (tv g ++ tv te)) bs (SConj ([c] ++ cs)) (te ~~ a)
                                 let f' = Conj ([apply u f] ++ [fe])
                                 return ((apply u b) --> te, f')

conGenPat g a p _ = do (t,c,_,b) <- conGenPat' g p
                       return (t,Simp c)
