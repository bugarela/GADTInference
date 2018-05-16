module ConstraintGenerator where

import Head
import Type
import ConstraintSolver
import Data.List
import Lcg

conGen :: ([Assump]) -> Expr -> TI (SimpleType,GConstraint)

--VAR
conGen g (Var x) = do (t,c) <- tiContext g x
                      return (t, Proper (Simp c))

--CON
conGen g (Con i) = do (t,c) <- tiContext g i
                      return (t, Proper (Simp c))

-- IF
conGen g (If x t e) = do (tx,gx) <- conGen g x
                         (tt,gt) <- conGen g t
                         (te,ge) <- conGen g e
                         return (te, (GConj ([tx ~~ (TCon "Bool")] ++ [tt ~~ te] ++ [gx] ++ [gt] ++ [ge])))

--APP
conGen g (App e1 e2) = do (t1,g1) <- conGen g e1
                          (t2,g2) <- conGen g e2
                          a <- freshVar
                          let gr = GConj ([g1] ++ [g2] ++ [(t1 ~~ (t2 --> a))])
                          return (a, gr)

--ABS
conGen g (Lam i e) = do a <- freshVar
                        (t,gr) <- conGen (g /+/ [i:>:(convert a)]) e
                        return (a --> t , gr)

--LET
conGen g (Let (i,e1) e2) = do (t,g1) <- conGen g e1
                              s <- solveAll g1
                              let t' = apply s t
                              let q = quantify (tv t' \\ tv (apply s g)) t'
                              (vs,q') <- (freshSubstC q)
                              let bs = tv vs
                              (v,g2) <- conGen (g /+/ [i:>:(convert (inst vs q'))]) e2
                              let gr = GConj ([g2] ++ [Proper (Impl (map makeTvar (tv g)) bs E (instC vs g1))])
                              return (v,gr)

--LETA
conGen g (LetA (i,(Constrained t cs),e1) e2) = do t1 <- freshInstance t
                                                  (t',g1) <- conGen (g /+/ [i:>:(Constrained t cs)]) e1
                                                  (v,g2) <- conGen (g /+/ [i:>:(Constrained t cs)]) e2
                                                  let f1 = retrieveConstraints g1
                                                      f2 = retrieveConstraints g2
                                                  let gr = GConj ([Proper f2] ++ [Proper (Impl (map makeTvar (tv g)) (tv t) E (Proper f1))] ++ [t1 ~~ t'])
                                                  return (v,gr)

--CASE
conGen g (Case e ls) = do (te,ge) <- conGen g e
                          a <- freshVar
                          fs <- mapM (conGenAlt g a te) ls
                          let gs = map fst fs
                          let cs = map snd fs
                          let gr = GConj ([ge] ++ [Group gs] ++ cs)
                          return (a,gr)

conGenAlt g a te (p,e) = do (t, fi) <- conGenPat g a p e
                            let ti = leftArr t
                                vi = rightArr t
                            return ((ti --> vi,fi), te ~~ ti)


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
                                 (te,ge) <- conGen g' e
                                 let bs = findBs ta (map idOf (as ++ [b]))
                                 let as' = intersect as (map makeTvar (tv (rightArr t)))
                                 let f = Proper (Impl ([a] ++ as' ++ map makeTvar (tv g ++ tv te)) bs (SConj ([c] ++ cs)) (GConj ([(te ~~ a)] ++ [ge])))
                                 let f' = GConj ([apply u f] ++ [ge])
                                 return ((apply u b) --> te, f')

conGenPat g a p _ = do (t,c,_,b) <- conGenPat' g p
                       return (t,Proper (Simp c))
