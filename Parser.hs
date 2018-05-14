module Parser where
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Head
import Type

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

parseExpr e = parse expr "Error:" e

parseFile a = do f <- readFile a
                 let ls = lines f
                 let fs = (filter (notComment) ls)
                 r <- parseFile' fs
                 return (r)

parseFile' f = do let ds = map (parse dd "Error:") (init f)
                  let e = parse expr' "Error:" (last f)
                  return (ds,e)

reservados = "$.&|=->{},;()\n "
operators = map varof ["+","-","*","/","==",">","<",">=","<=","===","&&","||"]
opsymbols = "><=-+*/|&"

notComment (' ':s) = notComment s
notComment ('-':('-':_)) = False
notComment [] = False
notComment _ = True

varof c = Var c

expr' :: Parsec String () (Expr)
expr' = do v <- pvar
           spaces
           char '='
           spaces
           e <- expr
           return e

expr :: Parsec String () (Expr)
expr = do l <- lam
          return l
       <|>
       do apps <- many1 term
          return $ foldl1 App apps
       <|>
       do {t <- tuple; return t}
       <|> do char '('
              spaces
              e <- expr
              char ')'
              spaces
              return e


term :: Parsec String () (Expr)
term = do {i <- ifex; return i}
       <|>
       do {c <- caseex; return c}
       <|>
       do {l <- letex; return l}
       <|>
       do {a <- apps; return a}

alt :: Parsec String () (Pat, Expr)
alt = do spaces
         p <- pat
         string "->"
         spaces
         e <- expr
         return (p,e)

pat :: Parsec String () (Pat)
pat = do {p <- pcon; return p}
      <|>
      do {p <- pvar; return p}
      <|>
      do {t <- tuplePat; return t}


lam :: Parsec String () (Expr)
lam = do char '\\'
         spaces
         var <- varName
         spaces
         string "->"
         spaces
         e <- expr
         return (Lam var e)

ifex :: Parsec String () (Expr)
ifex = do string "if "
          e1 <- singleExpr
          string "then "
          e2 <- singleExpr
          string "else "
          e3 <- singleExpr
          return (If e1 e2 e3)

caseex :: Parsec String () (Expr)
caseex = do string "case "
            e <- singleExpr
            string "of {"
            ps <- alt `sepBy` (char ';')
            char '}'
            return (Case e ps)

letex :: Parsec String () (Expr)
letex = do try $ do string "let "
                    spaces
                    v <- many1 (noneOf reservados)
                    spaces
                    char '='
                    spaces
                    e <- singleExpr
                    spaces
                    string "in "
                    spaces
                    e' <- expr
                    return (Let (v,e) e')
        <|>
        do string "let "
           spaces
           v <- many1 (noneOf reservados)
           spaces
           string "::"
           spaces
           a <- typeScheme
           spaces
           char '='
           spaces
           e <- expr
           spaces
           string "in "
           spaces
           e' <- expr
           return (LetA (v,(quantifyAll a),e) e')

apps :: Parsec String () (Expr)
apps = do as <- many1 singleExpr
          return (foldApp as)

singleExpr :: Parsec String () (Expr)
singleExpr = do try $ do char '('
                         e <- expr
                         char ')'
                         spaces
                         return e
             <|>
             do var <- varReservada
                return (var)
             <|>
             do var <- varName
                spaces
                return (Var var)
             <|>
             do {t <- tuple; return t}

con :: Parsec String () (Expr)
con = do c <- conName
         return (Con c)

pvar :: Parsec String () (Pat)
pvar = do var <- varName
          spaces
          return (PVar var)

pcon :: Parsec String () (Pat)
pcon = do c <- conName
          ps <- many pat
          return (PCon c ps)

varReservada :: Parsec String () (Expr)
varReservada = do {con <- conName; return (Con con)}
               <|>
               do op <- many1 (oneOf opsymbols)
                  spaces
                  return (varof op)

conName :: Parsec String () ([Char])
conName = do a <- oneOf ['A'..'Z']
             as <- many (noneOf reservados)
             spaces
             return ([a] ++ as)
          <|>
          do try $ do as <- many1 digit
                      spaces
                      return (['0'] ++ as)

varName :: Parsec String () ([Char])
varName = do a <- noneOf (['A'..'Z'] ++ ['1'..'9'] ++ reservados)
             as <- many (noneOf reservados)
             spaces
             let s = [a] ++ as
             return ([a] ++ as)

foldApp :: [Expr] -> Expr
foldApp [x] = x
foldApp [f,g] = if g `elem` operators then (App g f) else (App f g)
foldApp (f:(g:as)) = if g `elem` operators then App (App g f) (foldApp as) else App (App f g) (foldApp as)

dd :: Parsec String () [Assump]
dd = do try $ do {a <- adt; return a}
     <|>
     do try $ do {a <- gadt; return a}

adt :: Parsec String () [Assump]
adt = do string "data"
         spaces
         i <- conName
         spaces
         ps <- many tvar
         char '='
         rs <- tcon `sepBy` (char '|')
         return (map (buildADT i ps) rs)

gadt :: Parsec String () [Assump]
gadt = do string "data"
          spaces
          i <- conName
          spaces
          ps <- manyTill tvar (try (string "where"))
          spaces
          char '{'
          spaces
          rs <- gtcon `sepBy` (char ';')
          char '}'
          return (toGADT rs)

tvar :: Parsec String () (SimpleType)
tvar = do var <- varName
          spaces
          return (TVar var)

tcon :: Parsec String () (([Char],[SimpleType]))
tcon = do spaces
          c <- conName
          spaces
          vs <- many tParam
          spaces
          return (c,vs)

tParam :: Parsec String () SimpleType
tParam = do c <- conName
            spaces
            return (TCon c)
         <|>
         do t <- tvar
            return t
         <|>
         do char '('
            spaces
            t <-tParam
            spaces
            char ')'
            spaces
            return t

gtcon :: Parsec String () (Assump)
gtcon = do spaces
           c <- conName
           string "::"
           spaces
           cs <- do cs <- constraint
                    spaces
                    string "=>"
                    spaces
                    return cs
                 <|> do return E
           s <- typeScheme
           spaces
           return (c :>: quantifyAllC s cs)

typeScheme :: Parsec String () SimpleType
typeScheme = do try $ do t <- typeScheme'
                         spaces
                         string "->"
                         spaces
                         t' <- typeScheme
                         return (TArr t t')
             <|> typeScheme'

typeScheme' :: Parsec String () SimpleType
typeScheme' = do try $ do ts <- many1 singleType
                          return (foldl1 TApp ts)
             <|>
             do singleType

singleType :: Parsec String () SimpleType
singleType = do try $ do char '('
                         spaces
                         t <- typeScheme
                         char ')'
                         spaces
                         return t
             <|>
             do try $ do {c <- tParam; return c}
             <|>
             do {t <- tupleType; return t}

buildADT i ps (c,vs) = c :>: quantifyAll (foldl1 TArr (vs ++ [foldl1 TApp ([TCon i]++ps)]))

constraint :: Parsec String () SConstraint
constraint = do cs <- singleConstraint `sepBy` (char '^')
                return (SConj cs)

singleConstraint :: Parsec String () SConstraint
singleConstraint = do char 'e'
                      spaces
                      return (E)
                   <|>
                   do char '('
                      spaces
                      t <- singleType
                      spaces
                      char '~'
                      spaces
                      t' <- singleType
                      spaces
                      char ')'
                      spaces
                      return (TEq t t')

tuple :: Parsec String () Expr
tuple = do char '('
           spaces
           es <- (do {spaces; e <- expr; return e}) `sepBy` (char ',')
           spaces
           char ')'
           spaces
           return (foldl1 App ([Con (idTup (length es))] ++ es))

tuplePat :: Parsec String () Pat
tuplePat = do char '('
              spaces
              es <- (do {spaces; p <- pat; return p}) `sepBy` (char ',')
              spaces
              char ')'
              spaces
              return (PCon (idTup (length es)) es)

tupleType :: Parsec String () SimpleType
tupleType = do char '('
               spaces
               es <- (do {spaces; t <- typeScheme; return t}) `sepBy` (char ',')
               spaces
               char ')'
               spaces
               return (foldl1 TApp ([TCon (idTup (length es))] ++ es))

idTup n = "(" ++ take (n-1) [',',','..] ++ ")"
