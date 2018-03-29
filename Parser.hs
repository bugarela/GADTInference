module Parser where
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Head
import Type

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

parseExpr e = parse expr "Erro:" e

parseFile = do f <- readFile "input.txt"
               let ls = lines f
               let ds = map (parse dd "Erro:") (init ls)
               let e = parse expr "Erro:" (last ls)
               return (ds,e)

reservados = ".|=->{},;()\n "
operators = map varof ["+","-","*","/","==",">","<",">=","<="]
opsymbols = "><=-+*/"

varof c = Var c

expr :: Parsec String () (Expr)
expr = do l <- lam
          return l
       <|>
       do apps <- many1 term
          return $ foldl1 App apps
       <|>
       do {t <- tuple; return t}


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
pat = do {p <- plit; return p}
      <|>
      do {p <- pcon; return p}
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
        do try $ do string "let "
                    spaces
                    v <- many1 (noneOf reservados)
                    spaces
                    string "::"
                    spaces
                    a <- typeScheme
                    spaces
                    char '='
                    spaces
                    e <- singleExpr
                    spaces
                    string "in "
                    spaces
                    e' <- expr
                    return (LetA (v,(quantify (tv a) a),e) e')

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
             do {t <- tuple; return t}
             <|>
             do l <- lit
                return l
             <|>
             do var <- varReservada
                return (var)
             <|>
             do var <- varName
                spaces
                return (Var var)

lit :: Parsec String () (Expr)
lit = do digits <- many1 digit
         let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
         spaces
         return (Lit (TInt (fromInteger n)))
      <|>
      do try $ do {string "True"}
         spaces
         return (Lit (TBool True))
      <|>
      do try $ do {string "False"}
         spaces
         return (Lit (TBool False))

con :: Parsec String () (Expr)
con = do c <- conName
         return (Con c)

pvar :: Parsec String () (Pat)
pvar = do var <- varName
          spaces
          return (PVar var)

plit :: Parsec String () (Pat)
plit = do digits <- many1 digit
          let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
          spaces
          return (PLit (TInt (fromInteger n)))
       <|>
       do try $ do {string "True"}
          spaces
          return (PLit (TBool True))
       <|>
       do try $ do {string "False"}
          spaces
          return (PLit (TBool False))

pcon :: Parsec String () (Pat)
pcon = do c <- conName
          ps <- many varName
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

varName :: Parsec String () ([Char])
varName = do a <- noneOf (['A'..'Z'] ++ reservados)
             as <- many (noneOf reservados)
             spaces
             let s = [a] ++ as
             return ([a] ++ as)

foldApp :: [Expr] -> Expr
foldApp [x] = x
foldApp [f,g] = if g `elem` operators then (App g f) else (App f g)
foldApp (f:(g:as)) = if g `elem` operators then App (App g f) (foldApp as) else App (App f g) (foldApp as)

dd :: Parsec String () [GADT]
dd = do try $ do {a <- adt; return a}
     <|>
     do try $ do {a <- gadt; return a}

adt :: Parsec String () [GADT]
adt = do string "data"
         spaces
         i <- conName
         spaces
         ps <- many tvar
         char '='
         rs <- tcon `sepBy` (char '|')
         return (map (buildADT i ps) rs)

gadt :: Parsec String () [GADT]
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
          return (rs)

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

tlit :: Parsec String () (SimpleType)
tlit = do try $ do {string "Int"}
          spaces
          return (TLit Int)
       <|>
       do try $ do {string "Bool"}
          spaces
          return (TLit Bool)

gtcon :: Parsec String () (GADT)
gtcon = do spaces
           c <- conName
           spaces
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
           return (c,quantifyAll s,cs)

singleType :: Parsec String () SimpleType
singleType = do {c <- tParam; return c}
             <|>
             do {l <- tlit; return l}

typeScheme :: Parsec String () SimpleType
typeScheme = do try $ do t <- singleType
                         spaces
                         t' <- typeScheme
                         return (TApp t t')
            <|>
            do try $ do t <- singleType
                        spaces
                        string "->"
                        spaces
                        t' <- typeScheme
                        return (TArr t t')
            <|> do t <- singleType
                   return t

buildADT i ps (c,vs) = (c,quantifyAll (foldl1 TArr (vs ++ [foldl1 TApp ([TCon i]++ps)])),E)

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
           es <- expr `sepBy` (char ',')
           spaces
           char ')'
           spaces
           return (foldl1 App ([Con (idTup (length es))] ++ es))

tuplePat :: Parsec String () Pat
tuplePat = do char '('
              spaces
              es <- varName `sepBy` (char ',')
              spaces
              char ')'
              spaces
              return (PCon (idTup (length es)) es)

idTup n = "(" ++ take (n-1) [',',','..] ++ ")"
