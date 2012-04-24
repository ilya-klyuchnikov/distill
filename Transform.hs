{-# LANGUAGE NPlusKPatterns #-}

module Transform where

import Expr
import Context
import Exception
import Data.List (find)
import Data.Maybe (isJust)
import Debug.Trace

transform n e@(Var _) k r s fv d = transformCtx n e k r s fv d
transform _ (Bound _) _ _ _ _ _ = error "Unexpected bound variable"
transform n (Lambda v e) EmptyCtx r s fv d = do
    e' <- transform n (subst 0 (Var v') e) EmptyCtx r s (v':fv) d
    return (Lambda v (abstract 0 v' e'))
 where
    v' = renamevar fv v
transform n (Lambda _ e) (AppCtx k e') r s fv d = transform n (subst 0 e' e) k r s fv d
transform _ (Lambda{}) (CaseCtx{}) _ _ _ _ = error "Unapplied function in case selector"
transform n (Con c es) EmptyCtx r s fv d = do
    es' <- mapM (\e -> transform n e EmptyCtx r s fv d) es
    return (Con c es')
transform _ e@(Con{}) (AppCtx{}) _ _ _ _ = error ("Constructor application is not saturated: " ++ show e)
transform n e@(Con c es) (CaseCtx k bs) r s fv d = case find (\(Branch c' vs e) -> c == c' && length vs == length es) bs of
    Nothing -> error ("No matching pattern in case for term:\n\n" ++ show (Case e bs))
    Just (Branch _ _ e) -> transform n (foldr (subst 0) e es) k r s fv d
transform n (App e e') k r s fv d = transform n e (AppCtx k e') r s fv d
transform 0 ef@(Func f) k r s fv d = trace (show e) (case find (\e' -> isJust (inst e' e [])) r of
    Just e' -> if   isRenaming s'
                then return (Fold f e)
                else transform 0 (extract s' s e') EmptyCtx r s fv d
            where
                (Just s') = inst e' e []
    Nothing -> case find (\e' -> isJust (couple e' e [])) r of
        Just e' -> throw (e',e)
        Nothing -> case lookup f d of
            Nothing -> error ("Undefined function: "++f)
            Just e' -> handle t handler
                    where
                       t = do
                               t' <- transform 0 e' k (e:r) s fv d
                               return (Unfold f e t')
                       handler (t, t')
                        | e == t = let (t'',s') = generalise t t' [] fv []
                                   in  transform 0 (extract s' s t'') EmptyCtx r s fv d
                        | otherwise = throw (t, t'))
 where
    e = place ef k
transform (n+1) (Func f) k r s fv d = do
    e <- transform n (Func f) k [] s fv d
    trace (show e) (case find (\e' -> isJust (inst e' e [])) r of
        Just e' -> if   isRenaming s'
                    then return (Fold f e)
                    else transform (n+1) (extract s' s e') EmptyCtx r s fv d
                where
                    (Just s') = inst e' e []
        Nothing -> case find (\e' -> isJust (couple e' e [])) r of
            Just e' -> throw (e',e)
            Nothing -> handle t handler
                    where
                       t = do
                             t' <- transform (n+1) e EmptyCtx (e:r) s fv d
                             return (Unfold f e t')
                       handler (t, t')
                        | e == t = let (t'',s') = generalise t t' [] fv []
                                   in  transform (n+1) (extract s' s t'') EmptyCtx r s fv d
                        | otherwise = throw (t, t'))
transform n (Case e bs) k r s fv d = transform n e (CaseCtx k bs) r s fv d
transform n e@(Let v t u) k r s fv d = handle e' handler
    where
         v' = renamevar fv v
         e' = do
              t' <- transform n t EmptyCtx r s fv d
              u' <- transform n (subst 0 (Var v') u) k (e:r) ((v', t):s) (v':fv) d
              return (Let v t' (abstract 0 v' u'))
         handler (t, t')
          | e == t = let (t'',s') = generalise t t' [] fv []
                     in  transform n (extract s' s t'') EmptyCtx r s fv d
          | otherwise = throw (t, t')
transform n t@(Unfold f t' u) k r s fv d = handle e handler
    where
        e = do
               u' <- transform n u k (t:r) s fv d
               return (Unfold f t u')
        handler (e, e')
         | e == t = let (e'',s') = generalise e e' [] fv []
                    in  transform n (extract s' s e'') EmptyCtx r s fv d
         | otherwise = throw (e, e')
transform n (Fold f t) k r s fv d = case find (\t' -> isJust (inst t' t [])) r of
    Just t' -> if   isRenaming s'
                then return (Fold f t)
                else transform n (extract s' s t') k r s fv d
            where
               (Just s') = inst t' t []
    Nothing -> case find (\t' -> isJust (couple t' t [])) r of
        Just t' -> throw (t',t)
        Nothing -> handle e handler
                where
                   e = do
                        e' <- transform n t k (t:r) s fv d
                        return (Unfold f t e')
                   handler (e, e')
                    | e == t = let (e'',s') = generalise e e' [] fv []
                               in  transform n (extract s' s e'') k r s fv d
                    | otherwise = throw (e, e')

transformCtx n e EmptyCtx r s fv d = return e
transformCtx n e (AppCtx k e') r s fv d = do 
    e'' <- transform n e' EmptyCtx r s fv d
    transformCtx n (App e e'') k r s fv d
transformCtx n e (CaseCtx k bs) r s fv d = do 
    bs' <- mapM (\(Branch c vs e') -> let e'' = place e' k
                                          fv' = foldr (\v fv -> let v' = renamevar fv v in v':fv) fv vs
                                          vs' = take (length vs) fv'
                                          t = foldr (\v e -> subst 0 (Var v) e) (replace e (Con c (map Var vs')) e'') vs'
                                      in do 
                                          t' <- transform n t EmptyCtx r s fv' d
                                          return (Branch c vs $ foldl (flip (abstract 0)) t' vs')) bs
    return (Case e bs')