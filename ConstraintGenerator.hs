module ConstraintGenerator where

import Head
import Type
import ConstraintSolver
import Data.List
import Lcg

conGen :: ([Assump]) -> Expr -> TI (SimpleType, Constraint, Subst)

--VAR
conGen g (Var x) = do (t,c) <- tiContext g x
                      return (t, Simp c, [])

--CON
conGen g (Con i) = do (t,c) <- tiContext g i
                      return (t, Simp c, [])

-- IF
conGen g (If x t e) = do (tx,gx,sx) <- conGen g x
                         (tt,gt,st) <- conGen g t
                         (te,ge,se) <- conGen g e
                         return (te, (Conj ([tx ~~ (TCon "Bool")] ++ [tt ~~ te] ++ [gx] ++ [gt] ++ [ge])), se @@ st @@ sx)

--APP
conGen g (App e1 e2) = do (t1,f1,s1) <- conGen g e1
                          (t2,f2,s2) <- conGen g e2
                          a <- freshVar
                          let f = Conj ([f1] ++ [f2] ++ [(t1 ~~ (t2 --> a))])
                          return (a, f, s2 @@ s1)

--ABS
conGen g (Lam i e) = do a <- freshVar
                        (t,f,s) <- conGen (g /+/ [i:>:(convert a)]) e
                        return (a --> t , f, s)

--LET
conGen g (Let (i,e1) e2) = do (t,f1,s1) <- conGen g e1
                              ss <- solver (simple (apply s1 f1))
                              let s = ss @@ s1
                                  t' = apply s t
                                  q = quantify (tv t' \\ tv (apply s g)) t'
                              --error (show t')
                              (vs,q') <- (freshSubstC q)
                              let bs = tv vs
                              (v,f2,s2) <- conGen (g /+/ [i:>:(convert (inst vs q'))]) e2
                              --let fs = Conj ([f2] ++ [Impl (map makeTvar (tv g)) [] E (apply s (instC vs f1))])
                              let s' = s2 @@ s
                              return (apply s' v, f2, s2)

--LETA
conGen g (LetA (i,(Constrained t cs),e1) e2) = do t1 <- freshInstance t
                                                  (t',f1,_) <- conGen (g /+/ [i:>:(Constrained t cs)]) e1
                                                  (v,f2,s2) <- conGen (g /+/ [i:>:(Constrained t cs)]) e2
                                                  let gr = Conj ([f2] ++ [(Impl (map makeTvar (tv g)) (tv t) E f1)] ++ [t1 ~~ t'])
                                                  return (v,gr,s2)

--CASE
conGen g (Case e ls) = do (te,fe,se) <- conGen g e
                          a <- freshVar
                          ps <- mapM (conGenAlt g) ls
                          sa <- solveGroups (map (buildGroup a) ps)
                          let fs = map (buildConstraint a te) ps
                              f = Conj ([fe] ++ fs)
                              ss = getSubs ps
                              s = sa @@ ss @@ se
                          return (a,f,s)


conGenPats g [] = return ([],[],g,[])
conGenPats g (p:ps) = do (t1,c1,g1,b1) <- conGenPat g p
                         (t2,c2,g2,b2) <- conGenPats g1 ps
                         return ([t1] ++ t2, [c1] ++ c2, (g /+/ g1) /+/ g2, b1 ++ b2)

conGenPat g (PVar i) = do b <- freshVar
                          return (b, E, (g /+/ [i:>:(convert b)]), [b])
conGenPat g (PCon i []) = do (t,c') <- tiContext g i
                             return (t, E, g, [])
conGenPat g (PCon i xs) = do (t,c) <- tiContext g i
                             (ts,cs,gp,as) <- conGenPats g xs
                             b <- freshVar
                             let ta = foldr1 TArr (ts ++ [b])
                             let u = unify t ta
                             let as' = intersect as (map makeTvar (tv (rightArr t)))
                             let g' = apply u gp
                             return (apply u b, apply u (SConj ([c] ++ cs)), g',as)

conGenAlt g ((PCon i xs),e) = do (t,c) <- tiContext g i
                                 (ts,cs,gp,as) <- conGenPats g xs
                                 b <- freshVar
                                 let ta = foldr1 TArr (ts ++ [b])
                                 let u = unify t ta
                                 let g' = apply u gp
                                 (te,fe,se) <- conGen g' e
                                 let bs = findBs ta (map idOf (as ++ [b]))
                                 let as' = apply u ((intersect as (map makeTvar (tv (rightArr t)))) ++ map makeTvar (tv g ++ tv te))
                                 let d = apply u (SConj ([c] ++ cs))
                                 let fe' = apply u fe
                                 --let f = Impl ([a] ++ as' ++ map makeTvar (tv g ++ tv te)) bs (SConj ([c] ++ cs)) (Conj ([a ~~ te] ++ [fe]))
                                 --let f' = Conj ([apply u f] ++ [fe])
                                 return ((apply u b) --> te, as', bs, d, fe', se)

conGenAlt g (p,_) = do (t,c,_,b) <- conGenPat g p
                       return (t, [], [], c, Simp E, [])
