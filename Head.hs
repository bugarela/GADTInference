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
                | Impl [SimpleType] [Id] SConstraint GConstraint
                | Conj [Constraint]
                deriving Eq

data GConstraint = Proper Constraint
                 | Group [(SimpleType,GConstraint)]
                 | GConj [GConstraint]
                 deriving Eq

instance Show SimpleType where
    show (TVar i) = i
    show (TArr (TArr a b) t') = "("++show (TArr a b)++")"++"->"++show t'
    show (TArr t t') = "(" ++ show t++"->"++show t'++")"
    show (TCon i) = i
    show (TApp c v) = showApp (listApp c v)
    show (TGen n) = "tg" ++ show n

instance Show SConstraint where
    show (TEq t t') = "(" ++ show t ++ " ~ " ++ show t' ++ ")"
    show (Unt as bs c) = show as ++ "(Forall " ++ show bs ++ "." ++ showInLine c ++ ")"
    show E = "e"
    show (SConj [c]) = show c
    show (SConj (c:cs)) = show c ++ "\n" ++ show (SConj cs)

instance Show Constraint where
    show (Simp c) = show c
    show (Impl as bs c g) = show as ++ "(Forall " ++ show bs ++ "." ++ showInLine c ++ " imp " ++ showInLine' g ++ ")"
    show (Conj [c]) = show c
    show (Conj (c:cs)) = show c ++ "\n" ++ show (Conj cs)

instance Show GConstraint where
    show (Proper f) = show f
    show (Group [g]) = "-" ++ show g
    show (Group (g:gs)) = "-" ++ show g ++ "\n" ++ show (Group gs)
    show (GConj [c]) = show c
    show (GConj (c:cs)) = show c ++ "\n" ++ show (GConj cs)

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

showInLine (SConj [c]) = show c
showInLine (SConj (c:cs)) = show c ++ " ^ " ++ showInLine (SConj cs)
showInLine c = show c

showInLine' (GConj [c]) = show c
showInLine' (GConj (c:cs)) = show c ++ " ^ " ++ showInLine' (GConj cs)
showInLine' c = show c
