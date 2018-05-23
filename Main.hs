import Head
import Type
import Parser
import ConstraintGenerator
import ConstraintSolver

solve' g e = runTI (generateAndSolve (context /+/ g) e)

generate' g e = cleanConstraints (runTI (conGen (context /+/ g) e))

generateAndSolve g e = do (t,cs,s) <- conGen g e
                          u <- solveAll (clean (apply s cs))
                          --error (show s ++ show u ++ show t ++ show cs)
                          return (apply (u @@ s) t)

solve a = do as <- parseFile a
             inferFile' solve' as
             return()

generate a = do as <- parseFile a
                inferFile' generate' as
                return()

inferFile' f (ds,e) = case e of
                      Left err -> print err
                      Right e -> case (extract ds) of
                                    Right s -> print (f (fold s) e)
                                    Left errs -> print errs


-- ugly stuff down here
fromRight (Right x) = x

extract ds = if (extract' ds) then Right (map fromRight ds) else Left ([extractErr ds])

extract' [] = True
extract' (d:ds) = case d of
                  Left err -> False
                  Right a -> True && (extract' ds)

extractErr (d:ds) = case d of
                     Left err -> err
                     Right a -> extractErr ds
