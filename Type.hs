module Type where
import Data.List
import Data.Maybe(fromJust)
import Head


--------------------------
instance Functor TI where
   fmap f (TI m) = TI (\e -> let (a, e') = m e in (f a, e'))

instance Applicative TI where
    pure a = TI (\e -> (a, e))
    TI fs <*> TI vs = TI (\e -> let (f, e') = fs e; (a, e'') = vs e' in (f a, e''))

instance Monad TI where
    return x = TI (\e -> (x, e))
    TI m >>= f  = TI (\e -> let (a, e') = m e; TI fa = f a in fa e')

freshVar :: TI SimpleType
freshVar = TI (\e -> let v = "t"++show e in (TVar v, e+1))

runTI (TI m) = let (t, _) = m 0 in t

----------------------------
(/+/)      :: [Assump] -> [Assump] -> [Assump]
a1 /+/ a2    = nubBy assumpEq (a2 ++ a1)

assumpEq (x:>:_) (u:>:_) = (x == u)

t --> t' = TArr t t'

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

infixr 5 ~~
(~~) :: SimpleType -> SimpleType -> GConstraint
t1 ~~ t2 = Proper (Simp (TEq t1 t2))

properImpl :: [SimpleType] -> [Id] -> SConstraint -> Constraint -> GConstraint
properImpl as bs c f = Proper (Impl as bs c (Proper f))
----------------------------
class Subs t where
  apply :: Subst -> t -> t
  tv    :: t -> [Id]

instance Subs SimpleType where
  apply s (TVar u)  =
                    case lookup u s of
                       Just t  -> t
                       Nothing -> TVar u
  apply s (TCon u)  =
                    case lookup u s of
                       Just t  -> t
                       Nothing -> TCon u

  apply s (TArr l r) =  TArr (apply s l) (apply s r)
  apply s (TApp c v) =  TApp (apply s c) (apply s v)
  apply _ (TGen n) = TGen n
  apply _ (TSkolem n) = TSkolem n

  apply s (TGADT u)  =
                    case lookup u s of
                       Just t  -> t
                       Nothing -> TGADT u

  tv (TVar u)  = [u]
  tv (TArr l r) = tv l `union` tv r
  tv (TApp c v) = tv c `union` tv v
  tv _ = []

instance Subs a => Subs [a] where
  apply s     = map (apply s)
  tv          = nub . concat . map tv

instance Subs Assump where
  apply s (i:>:t) = i:>:apply s t
  tv (_:>:t) = tv t

instance Subs Type where
  apply s (Forall qt) = Forall (apply s qt)
  tv (Forall qt)      = tv qt

instance Subs SConstraint where
  apply s (TEq a b) = TEq (apply s a) (apply s b)
  apply s (SConj cs) = SConj (map (apply s) cs)
  apply s (Unt as bs c) = (Unt as bs (apply s c))
  apply s E = E

  tv _ = []

instance Subs Constraint where
  apply s (Simp c) = Simp (apply s c)
  apply s (Conj cs) = Conj (map (apply s) cs)
  apply s (Impl as bs c f) = (Impl as bs (apply s c) (apply s f))

  tv _ = []

instance Subs GConstraint where
  apply s (Proper c) = Proper (apply s c)
  apply s (Group gs) = Group (map (\(a,b) -> (apply s a, apply s b)) gs)
  apply s (GConj cs) = GConj (map (apply s) cs)

  tv _ = []

instance Subs ConstrainedType where
  apply s (Constrained a t) = Constrained a (apply s t)
  tv (Constrained _ t) = tv t

class Cons t where
  instC :: [SimpleType] -> t -> t
  simple :: t -> SConstraint
  groups :: t -> GConstraint
  clean :: t -> t
  clean' :: t -> [t]

instance Cons SConstraint where
  instC _ (E) = E
  instC fs (TEq t t') = (TEq (inst fs t) (inst fs t'))
  instC fs (Unt ts is cs) = (Unt (map (inst fs) ts) is (instC fs cs))
  instC fs (SConj cs) = (SConj (map (instC fs) cs))

  simple a = a

  groups _ = Proper (Simp E)

  clean c = if cls == [] then E else SConj cls where cls = (clean' c)

  clean' (SConj []) = []
  clean' (SConj cs) = foldr1 (++) (map clean' cs)
  clean' (E) = []
  clean' (Unt as bs c) = [Unt as bs (clean c)]
  clean' c = [c]

instance Cons Constraint where
  instC fs (Simp c) = (Simp (instC fs c))
  instC fs (Impl ts is cs g) = (Impl (map (inst fs) ts) is (instC fs cs) (instC fs g))
  instC fs (Conj cs) = (Conj (map (instC fs) cs))

  simple (Simp c) = c
  simple (Conj [c]) = simple c
  simple (Conj (c:cs)) = SConj ([simple c] ++ [simple (Conj cs)])
  simple (Impl as bs E g) = Unt as bs (simple g)
  simple (Impl as bs c g) = if (clean c == E) then Unt as bs (simple g) else E
  simple _ = E

  groups _ = Proper (Simp E) -- change for nested cases (maybe)

  clean (Conj cs) = Conj (clean' (Conj cs))
  clean (Simp c) = Simp (clean c)
  clean c = c

  clean' (Conj []) = []
  clean' (Conj cs) = foldr1 (++) (map clean' cs)
  clean' (Impl as bs c f) = [Impl as bs (clean c) (clean f)]
  clean' (Simp a) = if cls == E then [] else [Simp cls] where cls = (clean a)

instance Cons GConstraint where
  instC fs (Proper f) = (Proper (instC fs f))
  instC fs (Group ts) = (Group (map (\(t,g) -> (inst fs t, instC fs g)) ts))
  instC fs (GConj gs) = (GConj (map (instC fs) gs))

  simple (Proper f) = simple f
  simple (GConj [c]) = simple c
  simple (GConj (c:cs)) = SConj ([simple c] ++ [simple (GConj cs)])
  simple (Group gs) = simple (GConj (map snd gs)) -- maybe this shoul be E

  groups (Group a) = (Group a)
  groups (GConj gs) = GConj (map groups gs)
  groups (Proper _) = Proper (Simp E)

  clean (GConj cs) = GConj (clean' (GConj cs))
  clean (Proper c) = Proper (clean c)
  clean c = c

  clean' (GConj []) = []
  clean' (GConj cs) = foldr1 (++) (map clean' cs)
  clean' (Group gs) = [Group (map (\(t,g) -> (t,clean g)) gs)]
  clean' (Proper a) = if cls == Simp E then [] else [Proper cls] where cls = (clean a)



------------------------------------
varBind :: Id -> SimpleType -> Maybe Subst
varBind u t | t == TVar u   = Just []
            | t == TCon u   = Just []
            | t == TGADT u  = Just []
            | u `elem` tv t = Nothing
            | otherwise     = Just [(u, t)]

mgu (TSkolem _, _) = Just []
mgu (_,TSkolem _) = Just []
mgu (TArr l r,  TArr l' r') = do s1 <- mgu (l,l')
                                 s2 <- mgu ((apply s1 r),(apply s1 r'))
                                 return (s2 @@ s1)
mgu (TApp c v, TApp c' v')  = do s1 <- mgu (c,c')
                                 s2 <- mgu ((apply s1 v) ,  (apply s1 v'))
                                 return (s2 @@ s1)
mgu (TVar u,   t        )   =  varBind u t
mgu (t,        TVar u   )   =  varBind u t
mgu (TGADT u,  TCon t   )   =  if u==t then Just [] else Nothing
mgu (TCon u,   TGADT t  )   =  if u==t then Just [] else Nothing
mgu (u,        t        )   =  if u==t then Just [] else Nothing

unify t t' =  case mgu (t,t') of
    Nothing -> error ("unification: trying to unify\n" ++ show t ++ "\nand\n" ++ show t')
    Just s  -> s

unifyAll v [] = []
unifyAll v (t:ts) = (unifyAll (apply s v) ts) @@ s where s = unify v t

tiContext :: [Assump] -> Id -> TI (SimpleType, SConstraint)
-- n-tuple on context
tiContext g ('(':is) = do t <- nTupleType is
                          r <- freshInstC t E
                          return (r)

-- numbers are marked with 0 and typed as Int
tiContext g ('0':is) = do a <- freshVar
                          return (a, TEq a (TCon "Int"))

tiContext g i = if l /= [] then (freshInst t c) else error ("Variable " ++ i ++ " undefined on context:" ++ show g ++ "\n")
    where
        l = dropWhile (\(i' :>: _) -> i /= i' ) g
        (_ :>: Constrained t c) = head l


check _ (Nothing) = False
check us (Just []) = True
check us (Just ((a,_):ss)) = if a `elem` us then False else check us (Just ss)

appParametros i [] = i
appParametros (TArr _ i) (_:ts) = appParametros i ts

convert a = Constrained (Forall a) E

quantify vs qt = Constrained (Forall (apply s qt)) (E) where
    vs' = [v | v <- tv qt, v `elem` vs]
    s = zip vs' (map TGen [0..])

quantifyC vs qt cs = Constrained (Forall (apply s qt)) (apply s cs) where
    vs' = [v | v <- tv qt, v `elem` vs]
    s = zip vs' (map TGen [0..])

quantifyAll t = quantify (tv t) t
quantifyAllC t cs = quantifyC (tv t) t cs

quantifyAssump (i,t) = i:>:quantifyAll t

skolemize :: [Id] -> SimpleType -> SimpleType
skolemize as t = let s = map (\i -> (i,TSkolem i)) as in apply s t

countTypes (TArr l r) = max (countTypes l) (countTypes r)
countTypes (TApp l r) = max (countTypes l) (countTypes r)
countTypes (TGen n) = n
countTypes _ = 0

freshInstance :: Type -> TI SimpleType
freshInstance (U t) = return t
freshInstance (Forall t) = do fs <- mapM (\_ -> freshVar) [0..(countTypes t)]
                              return (inst fs t)


freshSubst (Forall t) = do fs <- mapM (\_ -> freshVar) [0..(countTypes t)]
                           return (fs,t)

freshSubstC (Constrained (Forall t) _) = do fs <- mapM (\_ -> freshVar) [0..(countTypes t)]
                                            return (fs,t)

freshs (ts) = do fs <- mapM (\_ -> freshVar) ts
                 return (mkPair ts fs)

freshInst t c = do (fs,t') <- freshSubst t
                   return (inst fs t', instC fs c)

freshInstC t c = do (fs,t') <- freshSubstC t
                    return (inst fs t', instC fs c)

inst fs (TArr l r) = TArr (inst fs l) (inst fs r)
inst fs (TApp l r) = TApp (inst fs l) (inst fs r)
inst fs (TGen n) = fs !! n
inst _ t = t

retrieveConstraints (GConj gs) = Conj (map retrieveConstraints gs)
retrieveConstraints (Group gr) = retrieveConstraints (GConj (map snd gr))
retrieveConstraints (Proper a) = a

dom' (a, TVar b) = if a == b then "" else a
dom' (a,_) = a

dom a = (map dom' a)

toType a = U a

leftArr (TArr a (TArr b c)) = a --> leftArr (TArr b c)
leftArr (TArr a _) = a

rightArr (TArr a as) = rightArr as
rightArr (a) = a

cons (TGADT i) = i
cons (TCon i) = i
cons (TApp c _) = cons c

findAs (TApp c (TVar a)) = findAs c ++ [a]
findAs _ = []

findBs ps as = (tv ps) \\ as

gtv (TApp (TGADT i) (TVar a)) = [a]
gtv (TApp (TApp (TGADT i) (TVar a)) t) = findAs t ++ [a]
gtv (TApp a b) = gtv a ++ gtv b
gtv (TArr a b) = gtv a
gtv _ = []

toGADT [] = []
toGADT ((i :>: (Constrained (Forall a) c)):as) = (i :>: (Constrained (Forall (toGADT' a)) c)):toGADT as
toGADT' (TApp (TCon i) t) =(TApp (TGADT i) t)
toGADT' (TArr a b) = (TArr a (toGADT' b))
toGADT' a = a

makeTvar i = TVar i

idOf (TVar a) = a

mkPair [] _ = []
mkPair (a:as) (b:bs) = (a,b):mkPair as bs

nTupleType :: Id -> TI ConstrainedType
nTupleType i = do let n = length i
                  ts <- mapM (\_ -> freshVar) (take n [0..])
                  let r = (foldl1 TApp ([TCon ("(" ++ i)] ++ ts))
                  return (quantifyAll (foldr1 TArr (ts ++ [r])))

context = map quantifyAssump [("Just", TArr (TVar "a") (TApp (TCon "Maybe") (TVar "a"))),
           ("Nothing", TApp (TCon "Maybe") (TVar "a")),
           ("Left", TArr (TVar "a") (TApp (TApp (TCon "Either") (TVar "a")) (TVar "b"))),
           ("Right", TArr (TVar "a") (TApp (TApp (TCon "Either") (TVar "a")) (TVar "b"))),
           ("True", TCon "Bool"),
           ("False", TCon "Bool"),
           ("+", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Int"))),
           ("-", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Int"))),
           ("*", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Int"))),
           ("/", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Int"))),
           ("===", TArr (TVar "a") (TArr (TVar "a") (TCon "Bool"))),
           ("==", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Bool"))),
           (">=", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Bool"))),
           ("<=", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Bool"))),
           (">", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Bool"))),
           ("<", TArr (TCon "Int") (TArr (TCon "Int") (TCon "Bool"))),
           ("&&", TArr (TCon "Bool") (TArr (TCon "Bool") (TCon "Bool"))),
           ("||", TArr (TCon "Bool") (TArr (TCon "Bool") (TCon "Bool")))]

typeFromAssump (i:>:t) = t

-- foldr1 doesn't like empty lists
fold [] = []
fold (f:fs) = f ++ fold fs
