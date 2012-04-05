module Transform where

import Core.Expr
import Context
import Exception
import Data.List (find)
import Data.Maybe (isJust)

transform n e@(Var _) k r s fv d = transformCtx n e k r s fv d
transform _ (Bound _) _ _ _ _ _ = error "Unexpected bound variable"
transform n (Lambda x e) EmptyCtx r s fv d = 
    let 
        x' = renamevar fv x
    in do
        e' <- transform n (subst 0 (Var x') e) EmptyCtx r s (x':fv) d
        return (Lambda x (abstract 0 x' e'))
transform n (Lambda x e) (AppCtx k e') r s fv d = transform n (subst 0 e' e) k r s fv d
transform _ (Lambda{}) (CaseCtx{}) _ _ _ _ = error "Unapplied function in case selector"
transform n (Con c es) EmptyCtx r s fv d = do
   es' <- mapM (\e -> transform n e EmptyCtx r s fv d) es
   return (Con c es')
transform _ e@(Con{}) (AppCtx{}) _ _ _ _ = error ("Constructor application is not saturated: " ++ show e)
transform n e@(Con c es) (CaseCtx k bs) r s fv d = 
    case find (\(Pattern c' xs, _) -> c == c' && length xs == length es) bs of
        Just (_, e) -> transform n (foldr (subst 0) e es) k r s fv d
        _ -> error ("No matching pattern in case for term:\n\n"++show (Case e bs))
transform n (App e e') k r s fv d = transform n e (AppCtx k e') r s fv d
transform n (InfixApp e c e') k r s fv d = transform n e (InfixAppCtx k c e') r s fv d
transform 0 (Func f) k r s fv d = 
    let e = place (Func f) k
    in case find (\e' -> isJust (inst e' e [])) r of
        Just e' -> 
            let (Just s') = inst e' e []
            in if isRenaming s'
                then return (Fold f e)
                else transform 0 (extract s' e') EmptyCtx r s fv d
        _ -> case find (\e' -> isJust (couple e' e [])) r of
            Just e' -> throw (e', e)
            _ -> case lookup f d of
                    Just e' -> handle t handler
                            where
                               t = do
                                    t' <- transform 0 e' k (e:r) s fv d
                                    return (Unfold f e t')
                               handler (t, t')
                                | e == t = (\(t'', s') -> transform 0 (extract s' t'') EmptyCtx r s fv d) $ generalise t t' [] fv []
                                | otherwise = throw (t,t')
                    _ -> error ("Undefined function: " ++ f)
transform n (Func f) k r s fv d = do
    e <- transform (n - 1) (Func f) k [] s fv d
    case find (\e' -> isJust (inst e' e [])) r of
        Just e' -> 
            if isRenaming s'
                then return (Fold f e)
                else transform n (extract s' e') EmptyCtx r s fv d
            where
                (Just s') = inst e' e []
        _ -> case find (\e' -> isJust (couple e' e [])) r of
              Just e' -> throw (e', e)
              _ -> case lookup f d of
                    Just e' -> handle t handler
                            where
                               t = do
                                    t' <- transform 0 e' k (e:r) s fv d
                                    return (Unfold f e t')
                               handler (t, t')
                                | e == t = (\(t'', s') -> transform 0 (extract s' t'') EmptyCtx r s fv d) $ generalise t t' [] fv []
                                | otherwise = throw (t,t')
                    _ -> error ("Undefined function: "++f)
transform n (Case e bs) k r s fv d = transform n e (CaseCtx k bs) r s fv d
transform n (Let x e e') k r s fv d = 
    case find (\s -> e == snd s) s of
      Just s' -> transform n (subst 0 (Var (fst s')) e') k r s fv d
      _ -> do 
          t <- transform n e EmptyCtx r s fv d
          t' <- transform n (subst 0 (Var x') e') k r ((x',e):s) (x':fv) d
          return (Let x t (abstract 0 x' t')) 
        where
          x' = renamevar fv x   
transform n (Unfold f t u) k r s fv d = 
    case find (\t' -> isJust (inst t' t [])) r of
        Just t' -> 
            if isRenaming s'
                then return (Fold f t)
                else transform n (extract s' t') EmptyCtx r s fv d
            where
                (Just s') = inst t' t []
        _ -> case find (\t' -> isJust (couple t' t [])) r of
                Just t' -> throw (t',t)
                _ -> case lookup f d of
                        Just e -> handle e' handler
                                where
                                  e' = do
                                        t' <- transform 0 e k (t:r) s fv d
                                        return (Unfold f t t')
                                  handler (e, e')
                                   | e == t = (\(e'', s') -> transform 0 (extract s' e'') EmptyCtx r s fv d) $ generalise e e' [] fv []
                                   | otherwise = throw (e, e')
                        _ -> error ("Undefined function: "++f)
transform n (Typed e t) k r s fv d = transform n e k r s fv d >>= \e' -> return (Typed e' t)

transformCtx n e EmptyCtx r s fv d = return e
transformCtx n e (AppCtx k e') r s fv d = transform n e' EmptyCtx r s fv d >>= \e'' -> transformCtx n (App e e'') k r s fv d
transformCtx n e (CaseCtx k bs) r s fv d = do 
    bs' <- mapM (\(Pattern c xs,e') -> 
                    let 
                        e'' = place e' k
                        fv' = foldr (\x fv -> let x'=renamevar fv x in x':fv) fv xs
                        xs' = take (length xs) fv'
                        t = foldr (\x e -> subst 0 (Var x) e) (replace e (Con c (map Var xs')) e'') xs'
                    in do 
                        t' <- transform n t EmptyCtx r s fv' d
                        return (Pattern c xs,foldl (flip (abstract 0)) t' xs')) bs
    return (Case e bs')
                                              
