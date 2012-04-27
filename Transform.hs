{-# LANGUAGE NPlusKPatterns #-}

module Transform where

import Expr
import Context
import Exception
import Data.List
import Data.Maybe (isJust)
import Debug.Trace

transform n (Var x) k r s fv d = transformCtx n (Var x) k r s fv d
transform n (Bound i) k r s fv d = error ("Unexpected bound variable")
transform n (Lambda x e) EmptyCtx r s fv d = let x' = renamevar fv x
                                             in do
                                                e' <- transform n (subst 0 (Var x') e) EmptyCtx r s (x':fv) d
                                                return (Lambda x (abstract 0 x' e'))
transform n (Lambda x e) (AppCtx k e') r s fv d = transform n (subst 0 e' e) k r s fv d
transform n (Lambda x e) (CaseCtx k bs) r s fv d = error "Unapplied function in case selector"
transform n (Con c es) EmptyCtx r s fv d = do
                                           es' <- mapM (\e -> transform n e EmptyCtx r s fv d) es
                                           return (Con c es')
transform n (Con c es) (AppCtx k e) r s fv d = error ("Constructor application is not saturated: " ++ show (Con c es) ++ " " ++ show k)
transform n (Con c es) (CaseCtx k bs) r s fv d = case (find (\(Branch c' xs e) -> c==c' && length xs == length es) bs) of
                                                    Nothing -> error ("No matching pattern in case for term:\n\n"++show (Case (Con c es) bs))
                                                    Just (Branch c' xs e) -> transform n (foldr (\e e' -> subst 0 e e') e es) k r s fv d
transform n (App e e') k r s fv d = transform n e (AppCtx k e') r s fv d
transform 0 (Func f) k r s fv d = let e = place (Func f) k
                                  in  trace (show e) (case (find (\e' -> isJust (inst e' e [])) r) of
                                        Just e' -> let (Just s') = inst e' e []
                                                   in  if   isRenaming s'
                                                       then return (Fold f e)
                                                       else transform 0 (extract s' s e') EmptyCtx r s fv d
                                        Nothing -> case (find (\e' -> isJust (couple e' e [])) r) of
                                                      Just e' -> throw (e',e)
                                                      Nothing -> case (lookup f d) of
                                                                    Nothing -> error ("Undefined function: "++f)
                                                                    Just e' -> let t = do
                                                                                       t' <- transform 0 e' k (e:r) s fv d
                                                                                       return (Unfold f e t')
                                                                                   handler = \(t,t') -> if e==t then let (t'',s') = trace ("generalising func " ++ f) generalise t t' [] fv []
                                                                                                                     in  transform 0 (extract s' s t'') EmptyCtx r s fv d
                                                                                                                else throw (t,t')
                                                                               in  handle t handler)
transform (n+1) (Func f) k r s fv d = do
                                     e <- transform n (Func f) k [] s fv d
                                     trace (show e) (case (find (\e' -> isJust (inst e' e [])) r) of
                                        Just e' -> let (Just s') = inst e' e []
                                                   in  if   isRenaming s'
                                                       then return (Fold f e)
                                                       else transform (n+1) (extract s' s e') EmptyCtx r s fv d
                                        Nothing -> case (find (\e' -> isJust (couple e' e [])) r) of
                                                      Just e' -> throw (e',e)
                                                      Nothing -> let t = do
                                                                         t' <- transform (n+1) e EmptyCtx (e:r) s fv d
                                                                         return (Unfold f e t')
                                                                     handler = \(t,t') -> if e==t then let (t'',s') = trace ("generalising func " ++ f) generalise t t' [] fv []
                                                                                                       in  transform (n+1) (extract s' s t'') EmptyCtx r s fv d
                                                                                                  else throw (t,t')
                                                                 in  handle t handler)
transform n (Case e bs) k r s fv d = transform n e (CaseCtx k bs) r s fv d
transform n e@(Let x t u) k r s fv d = let x' = renamevar fv x
                                           e' = do
                                                t' <- transform n t EmptyCtx r s fv d
                                                u' <- transform n (subst 0 (Var x') u) k (e:r) ((x',t):s) (x':fv) d
                                                return (Let x t' (abstract 0 x' u'))
                                           handler = \(t,t') -> if e==t then let (t'',s') = trace ("generalising let " ++ show e) generalise t t' [] fv []
                                                                             in  transform n (extract s' s t'') EmptyCtx r s fv d
                                                                        else throw (t,t')
                                       in  handle e' handler
transform n t@(Unfold f t' u) k r s fv d = let e = do
                                                   u' <- transform n u k (t:r) s fv d
                                                   return (Unfold f t u')
                                               handler = \(e,e') -> if e==t then let (e'',s') = trace ("generalising unf: " ++ show t) generalise e e' [] fv []
                                                                                 in  transform n (extract s' s e'') EmptyCtx r s fv d
                                                                            else throw (e,e')
                                           in  handle e handler
transform n ee@(Fold f t) k r s fv d = case (find (\t' -> isJust (inst t' t [])) r) of
                                       Just t' -> let (Just s') = inst t' t []
                                                  in  if   isRenaming s'
                                                      then return (Fold f t)
                                                      else transform n (extract s' s t') k r s fv d
                                       Nothing -> case (find (\t' -> isJust (couple t' t [])) r) of
                                                     Just t' -> throw (t',t)
                                                     Nothing -> let e = do
                                                                        e' <- transform n t k (t:r) s fv d
                                                                        return (Unfold f t e')
                                                                    handler = \(e,e') -> if e==t then let (e'',s') = trace ("generalising fld: " ++ show ee) generalise e e' [] fv []
                                                                                                      in  transform n (extract s' s e'') k r s fv d
                                                                                                 else throw (e,e')
                                                                in  handle e handler
transform n (Typed e t) k r s fv d = do
    e' <- transform n e k r s fv d
    return (Typed e' t)
transform _ l@(Lit _) _ _ _ _ _ = return l

transformCtx n e EmptyCtx r s fv d = return e
transformCtx n e (AppCtx k e') r s fv d = do e'' <- transform n e' EmptyCtx r s fv d
                                             transformCtx n (App e e'') k r s fv d
transformCtx n e (CaseCtx k bs) r s fv d = do bs' <- mapM (\(Branch c xs e') -> let e'' = place e' k
                                                                                    fv' = foldr (\x fv -> let x' = renamevar fv x in x':fv) fv xs
                                                                                    xs' = take (length xs) fv'
                                                                                    t = foldr (\x e -> subst 0 (Var x) e) (replace e (Con c (map Var xs')) e'') xs'
                                                                                in do t' <- transform n t EmptyCtx r s fv' d
                                                                                      return (Branch c xs $ foldl (\e x -> abstract 0 x e) t' xs')) bs
                                              return (Case e bs')