module Lcg (lcg,lcgn,lcgn',lcgn2, lcgi) where

import Head
import Type


lcg:: [Type] -> TI SimpleType
lcg scs = do (qt, _) <- lcg' scs []; return qt

-----------------------------------------------------------------------------
type Gen = [((SimpleType,SimpleType),Id)]

lcg':: [Type] -> Gen -> TI (SimpleType, Gen)
lcg' [sc]     s = do qt <- freshInstance sc; return (qt,s)
lcg' (sc:scs) s = do ts <- mapM freshInstance scs
                     (t',s') <- lcgn' ts s
                     t0 <- freshInstance sc
                     (t,s'') <- lcgp t0 t' s'
                     return (t,s'')
lcg' []       _ = error "Lcg: empty list"

-----------------------------------------------------------------------------
lcgp:: SimpleType -> SimpleType -> Gen -> TI (SimpleType,Gen)
lcgp t1 t2 s =
  case lookup (t1,t2) s of
    Just a  -> return (TVar a, s)
    Nothing -> lcgp' t1 t2 s

lcgp':: SimpleType -> SimpleType -> Gen -> TI (SimpleType, Gen)
lcgp' t1@(TVar _)   t2            s = do TVar a <- freshVar; return (TVar a, ((t1,t2),a):s)
lcgp' t1            t2@(TVar _)   s = do TVar a <- freshVar; return (TVar a, ((t1,t2),a):s)
lcgp' t1@(TCon id1) t2@(TCon id2) s
  | id1==id2  = return (t1,s)
  | otherwise = do TVar a <- freshVar
                   return (TVar a, ((t1,t2),a):s)
lcgp' t1@(TApp t1a t1r) t2@(TApp t2a t2r) s =
  do (ta,s1) <- lcgp t1a t2a s
     (tr,s2) <- lcgp t1r t2r s1
     return (TApp ta tr, s2)
lcgp' t1@(TArr t1a t1r) t2@(TArr t2a t2r) s =
  do (ta,s1) <- lcgp t1a t2a s
     (tr,s2) <- lcgp t1r t2r s1
     return (TArr ta tr, s2)
lcgp' t t' s = do TVar a <- freshVar; return (TVar a, ((t,t'),a):s)

-----------------------------------------------------------------------------
lcgn:: [SimpleType] -> TI SimpleType
lcgn ts = do (t, _) <- lcgn' ts []; return t

lcgn':: [SimpleType] -> Gen -> TI (SimpleType,Gen)
lcgn' [t]        s = return (t,s)
lcgn' [t1, t2]   s = lcgp t1 t2 s
lcgn' (t1:t2:ts) s = do (ta,s1) <- lcgn' ts s; (tb,s2) <- lcgp t1 t2 s1; lcgp ta tb s2

lcgn2:: [SimpleType] -> Gen -> TI (SimpleType,Gen)
lcgn2 [t]        s = return (t,s)
lcgn2 [t1, t2]   s = lcgp t1 t2 s
lcgn2 (t1:t2:ts) s = do (ta,s1) <- lcgp t1 t2 s
                        if var ta then return (ta, s1)
                         else
                             do
                               (tb, s2) <- lcgn2 ts s1
                               lcgp ta tb s2

        where
           var (TVar _) = True
           var _        = False
-----------------------------------------------------------------------------
lcgS:: [SimpleType] -> [Subst] -> TI [SimpleType]
lcgS ts ss = do (ts,_) <- lcgS' ts ss []; return ts

lcgS' []      _  s = return ([],s)
lcgS' ts      [] s = return (ts,s)
lcgS' (t: ts) ss s =
 do (t1,s1) <- lcgn' ([ apply sj t | sj <- ss]) s
    (ti,s2)  <- lcgS' ts ss s1
    return (t1: ti, s2)

-----------------------------------------------------------------------------

lcgi:: [SimpleType] -> TI (Maybe SimpleType)
lcgi [] = return Nothing
lcgi ts = do t <- lcgn ts; return (Just t)

-- Apenas para testes
t1 = [Forall (TArr (TCon "Int") (TArr (TCon "Int") (TCon "Bool"))), Forall (TArr (TCon "Bool") (TArr (TCon "Bool") (TCon "Bool"))), Forall (TArr (TCon "Char") (TArr (TCon "Char") (TCon "Bool")))]
t2 = [Forall (TArr (TApp (TApp (TCon "(,)") (TCon "Int")) (TCon "Int")) (TCon "Bool")), Forall (TArr (TApp (TApp (TCon "(,)") (TCon "Bool")) (TCon "Bool")) (TCon "Bool"))]
t3 = [Forall (TArr (TApp (TApp (TApp (TCon "(,,)") (TCon "Int")) (TCon "Int")) (TCon "Bool")) (TCon "Bool")), Forall (TArr (TApp (TApp (TApp (TCon "(,,)") (TCon "Char")) (TCon "Char")) (TCon "Int")) (TCon "Int"))]


testlcg ts = runTI (lcg ts)
