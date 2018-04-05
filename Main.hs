import Head
import Type
import Parser
import ConstraintGenerator
import ConstraintSolver

solve' g e = runTI (solver (context ++ g) e)

generate' g e = snd (runTI (conGen (context ++ g) e))

solver g e = do (t,cs) <- conGen g e
                let u = solveAll cs
                return (apply u t)

solve a = do as <- parseFile a
             mapM (inferFile' solve') as
             return()

generate a = do as <- parseFile a
                mapM (inferFile' generate') as
                return()

inferFile' f (ds,e) = case e of
                      Left err -> print err
                      Right e -> case (extract ds) of
                                    Right s -> print (f (foldr1 (++) s) e)
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
