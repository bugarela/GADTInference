module Head where

type Index = Int
type Id = String
data TI a = TI (Index -> (a, Index))
type Subst  = [(Id, SimpleType)]
data Type = Forall SimpleType | U SimpleType deriving (Eq, Show)
data Assump = Id :>: ConstrainedType deriving (Eq, Show)
data ConstrainedType = Constrained Type SConstraint deriving (Eq, Show)

data SimpleType  = TVar Id
                 | TArr SimpleType SimpleType
                 | TCon Id
                 | TApp SimpleType SimpleType
                 | TGen Int
                 | TSkolem Id
                 | TGADT Id
                 deriving Eq

data Expr = Var Id
          | App Expr Expr
          | Lam Id Expr
          | Con Id
          | If Expr Expr Expr
          | Case Expr [(Pat,Expr)]
          | Let (Id,Expr) Expr
          | LetA (Id,ConstrainedType,Expr) Expr
          deriving (Eq, Show)

data Pat = PVar Id
         | PCon Id [Pat]
         deriving (Eq, Show)

data SConstraint = TEq SimpleType SimpleType
                 | Unt [SimpleType] [Id] SConstraint
                 | SConj [SConstraint]
                 | E
                 deriving Eq

data Constraint = Simp SConstraint
                | Impl [SimpleType] [Id] SConstraint Constraint
                | Conj [Constraint]
                deriving Eq

instance Show SimpleType where
    show (TVar i) = i
    show (TArr (TArr a b) t') = "("++show (TArr a b)++")"++"->"++show t'
    show (TArr t t') = "(" ++ show t++"->"++show t'++")"
    show (TCon i) = i
    show (TApp c v) = showApp (listApp c v)
    show (TGen n) = "tg" ++ show n
    show (TSkolem i) = "skolem" ++ show i
    show (TGADT i) = "GADT " ++ i

instance Show SConstraint where
    show (TEq t t') = "(" ++ show t ++ " ~ " ++ show t' ++ ")"
    show (Unt as bs c) = show as ++ "(Forall " ++ show bs ++ "." ++ showInLine c ++ ")"
    show E = "e"
    show (SConj [c]) = show c
    show (SConj (c:cs)) = show c ++ "\n" ++ show (SConj cs)

instance Show Constraint where
    show (Simp c) = showInLine c
    show (Impl as bs c f) = show as ++ "(Forall " ++ show bs ++ "." ++ showInLine c ++ " imp " ++ showInLine' f ++ ")"
    show (Conj [c]) = show c
    show (Conj (c:cs)) = show c ++ "\n" ++ show (Conj cs)

showApp [] = ""
showApp [x] = x
showApp (x:xs) = if (tup x) then "(" ++ showTuple (take n xs) ++ showApp (drop n xs) else x ++ " "++ showApp xs where
                      n = (length x - 1)

listApp (TApp a b) (TApp a' b') = (listApp a b) ++ ["(" ++ show (TApp a' b') ++ ")"]
listApp (TApp a b) v = (listApp a b) ++ [show v]
listApp v (TApp a b) = [show v] ++ ["(" ++ show (TApp a b) ++ ")"]
listApp c v = [show c] ++ [show v]

tup a = a !! 0 == '(' && a !! 1 == ','

showTuple [x] = x ++ ")"
showTuple (x:xs) = x ++ "," ++ showTuple xs

showInLine (SConj []) = ""
showInLine (SConj (E:cs)) = showInLine (SConj (filter notEmpty cs))
showInLine (SConj (c:cs)) = showInLine c ++ if rs /= SConj [] then " ^ " ++ showInLine rs else "" where rs = (SConj (filter notEmpty cs))
showInLine c = show c

showInLine' (Conj []) = ""
showInLine' (Conj ((Simp E):cs)) = showInLine' (Conj (filter notEmpty' cs))
showInLine' (Conj (c:cs)) = showInLine' c ++ if rs /= Conj [] then " ^ " ++ showInLine' rs else "" where rs = (Conj (filter notEmpty' cs))
showInLine' (Simp c) = showInLine c
showInLine' c = show c

notEmpty E = False
notEmpty (SConj cs) = (filter (notEmpty) cs) /= []
notEmpty _ = True

notEmpty' (Simp a) = notEmpty a
notEmpty' (Conj cs) = (filter (notEmpty') cs) /= []
notEmpty' _ = True
