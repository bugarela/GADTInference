module Head where

type Index = Int
type Id = String
data TI a = TI (Index -> (a, Index))
type Subst  = [(Id, SimpleType)]
data Type = Forall SimpleType | U SimpleType deriving (Eq, Show)
data Assump = Id :>: ConstrainedType deriving (Eq, Show)
data ConstrainedType = Constrained Type SConstraint deriving (Eq, Show)

data Literal = Int | Bool | TInt Int | TBool Bool deriving (Eq)

data SimpleType  = TVar Id
                 | TArr SimpleType SimpleType
                 | TLit Literal
                 | TCon Id
                 | TApp SimpleType SimpleType
                 | TGen Int
                 deriving Eq

data Expr = Var Id
          | App Expr Expr
          | Lam Id Expr
          | Lit Literal
          | Con Id
          | If Expr Expr Expr
          | Case Expr [(Pat,Expr)]
          | Let (Id,Expr) Expr
          | LetA (Id,ConstrainedType,Expr) Expr
          deriving (Eq, Show)

data Pat = PVar Id
         | PLit Literal
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
    show (TArr t t') = show t++"->"++show t'
    show (TCon i) = i
    show (TApp c v) = showApp (listApp c v)
    show (TLit tipo) = show tipo
    show (TGen n) = "tg" ++ show n

instance Show Literal where
    show (TInt _) = "Int"
    show (TBool _) = "Bool"
    show Int = "Int"
    show Bool = "Bool"

instance Show SConstraint where
    show (TEq t t') = "(" ++ show t ++ " ~ " ++ show t' ++ ")"
    show (Unt as bs c) = show as ++ "(Forall " ++ show bs ++ "." ++ show c ++ ")"
    show E = ""
    show (SConj [c]) = show c
    show (SConj (c:cs)) = show c ++ "\n" ++ show (SConj cs)

instance Show Constraint where
    show (Simp c) = show c
    show (Impl as bs c f) = show as ++ "(Forall " ++ show bs ++ "." ++ show c ++ " imp " ++ show f ++ ")"
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
