module Head where

type Index = Int
type Id = String
data TI a = TI (Index -> (a, Index))
type Subst  = [(Id, SimpleType)]
data Type = Forall SimpleType deriving (Eq, Show)
data Assump = Id :>: Type deriving (Eq, Show)
type GADT = (Id, Type, SConstraint)

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
          | LetA (Id,Type,Expr) Expr
          deriving (Eq, Show)

data Pat = PVar Id
         | PLit Literal
         | PCon Id [Id]
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
    show (TApp c v) = show c ++ " " ++ show v
    show (TLit tipo) = show tipo
    show (TGen n) = "tg" ++ show n

instance Show Literal where
    show (TInt _) = "Int"
    show (TBool _) = "Bool"
    show Int = "Int"
    show Bool = "Bool"

instance Show SConstraint where
    show (TEq t t') = show t ++ " ~ " ++ show t'
    show (Unt as bs c) = show as ++ "(Forall " ++ show bs ++ "." ++ show c ++ ")"
    show E = "e"
    show (SConj (c:cs)) = show c ++ " ^ " ++ show cs

instance Show Constraint where
    show (Simp c) = show c
    show (Impl as bs c f) = show as ++ "(Forall " ++ show bs ++ "." ++ show c ++ " imp " ++ show f ++ ")"
    show (Conj (c:cs)) = show c ++ " ^ " ++ show cs
