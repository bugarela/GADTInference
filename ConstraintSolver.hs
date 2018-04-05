module ConstraintSolver where

import Head
import Type
import Parser
import Data.List

solveAll cs = let u = sSolve (simple cs) in (sSolve' (apply u cs)) @@ u

sSolve' (Simp c) = sSolve c
sSolve' (Conj []) = []
sSolve' (Conj (c:cs)) = let s = sSolve' c in (sSolve' (Conj (map (apply s) cs))) @@ s
sSolve' (Impl as bs E f) = let s = sSolve' f in
                               if (intersect (tv (apply s as)) bs /= []) || (intersect bs (dom s) /= [])
                                 then error ("S-SImpl error with bs=" ++ show bs)
                                 else s
sSolve' (Impl as bs c f) = let p = sSolve c
                               t = solveAll (apply p f) in
                               if (intersect as (map makeTvar (dom t)) /= [])
                                 then error ("S-PImpl on :" ++ show (Impl as bs c f) ++ "with as= " ++ show as ++ " teta= " ++ show t)
                                 else t

sSolve :: SConstraint -> Subst
sSolve (TEq t u) = unify t u
sSolve (Unt as bs c) = let s = sSolve c in
                          if (intersect (tv (apply s as)) bs /= []) || (intersect bs (dom s) /= [])
                            then error ("S-SImpl error with bs=" ++ show bs)
                            else s
sSolve (SConj []) = []
sSolve (SConj (c:cs)) = let s = sSolve c in (sSolve (SConj (map (apply s) cs))) @@ s
sSolve E = []
