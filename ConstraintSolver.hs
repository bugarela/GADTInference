module ConstraintSolver where

import Head
import Type
import Parser
import Data.List
import Lcg

solveAll :: GConstraint -> TI Subst
solveAll gs = do ss <- solver (simple gs)
                 sg <- solver (apply ss (groups gs))
                 return (sg @@ ss)

class Solver t where
  solver :: t -> TI Subst

instance Solver SConstraint where
  solver (TEq t u) = do return (unify t u)
  solver (Unt as bs c) = do s <- solver c
                            if (intersect (tv (apply s as)) bs /= []) || (intersect bs (dom s) /= [])
                            then error ("S-SImpl error with bs=" ++ show bs)
                            else return s
  solver (SConj []) = do return []
  solver (SConj (c:cs)) = do s <- solver c
                             ss <- (solver (SConj (map (apply s) cs)))
                             return (ss @@ s)
  solver E = do return []

instance Solver Constraint where
  solver (Simp c) = solver c
  solver (Conj []) = do return []
  solver (Conj (c:cs)) = do s <- solver c
                            ss <- (solver (Conj (map (apply s) cs)))
                            return (ss @@ s)
  solver (Impl as bs E g) = do s <- solver g
                               if (intersect (tv (apply s as)) bs /= []) || (intersect bs (dom s) /= [])
                               then error ("S-SImpl error with bs=" ++ show bs)
                               else return s
  solver (Impl as bs c g) = do p <- solver c
                               t <- solveAll (apply p g)
                               if (intersect as (map makeTvar (dom t)) /= [])
                               then error ("S-PImpl on :" ++ show (Impl as bs c g) ++ "with as= " ++ show as ++ " teta= " ++ show t)
                               else return t

instance Solver GConstraint where
  solver (Proper f) = solver f
  solver (GConj []) = do return []
  solver (GConj (c:cs)) = do s <- solver c
                             ss <- (solver (GConj (map (apply s) cs)))
                             return (ss @@ s)
  solver (Group gr) = do let gs = map snd gr
                             ts = map fst gr
                         teta <- solveAll (GConj gs)
                         let vs = apply teta ts
                         t <- lcg (map toType vs)
                         let as = gtv (leftArr t)
                             sk = skolemize as t
                             u = unifyAll sk vs
                         return (unifyAll (apply u t) ts)

aaa :: [Type] -> [Type]
aaa a = a
