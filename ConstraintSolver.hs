module ConstraintSolver where

import Head
import Type
import Parser
import Data.List
import Lcg

solveAll fs = do s <- solver (simple fs)
                 sa <- solver (apply s fs)
                 return (sa @@ s)

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

solveGroups gr = do vs <- mapM solveAndApply gr
                    let ts = map (\(_,t,_) -> t) gr
                    t <- lcg (map toType vs)
                    let sk = skolemize t
                    let u = unifyAll sk vs
                    return (unifyAll (apply u t) ts)

solveAndApply (s,t,f) = do s' <- solver (simple f)
                           return (apply (s' @@ s) t)

solveImpl (t,(Impl _ _ c f))  = do s <- solver (SConj [c, simple f])
                                   return (apply s t)
solveImpl (t,(Simp (Unt _ _ c)))  = do s <- solver c
                                       return (apply s t)
solveImpl (t,Conj (f:fs)) = solveImpl (t,f)
solveImpl _ = error "solveImpl of wrong formated constraints"
