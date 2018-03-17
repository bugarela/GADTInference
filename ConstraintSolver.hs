import Head
import Type
import Parser
import ConstraintGenerator

solve' g e = runTI (solver (quantifiedContext g) e)

generate' g e = snd (runTI (conGen (quantifiedContext g) e))

solver g e = do (t,cs) <- conGen g e
                let u = unifyConstraints (simple cs) []
                return (apply u t)

solve = do a <- parseFile
           inferFile' a solve'
           return()

generate = do a <- parseFile
              inferFile' a generate'
              return()

inferFile' (ds,e) f = case e of
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
