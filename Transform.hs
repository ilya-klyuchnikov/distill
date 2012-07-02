{-# LANGUAGE NPlusKPatterns #-}

module Transform where

import Term
import Context
import Exception
import Data.List
import Data.Maybe (isJust)
import Debug.Trace

drive (Var x) k fv s d = case (lookup x s) of
                             Just t -> Subst t (drive t k fv s d)
                             Nothing -> driveCtx (Var x) k fv s d
drive (Lambda x t) EmptyCtx fv s d = let x' = renamevar fv x
                                     in Lambda x' (drive (rename [(x,x')] t) EmptyCtx (x':fv) s d)
drive (Lambda x t) (AppCtx k t') fv s d = let x' = renamevar fv x
                                          in drive (rename [(x,x')] t) k (x':fv) ((x',t'):s) d
drive (Lambda x t) (CaseCtx k bs) fv s d = error "Unapplied function in case selector"
drive (Con c ts) EmptyCtx fv s d = Con c (map (\t -> drive t EmptyCtx fv s d) ts)
drive (Con c ts) (AppCtx k t) fv s d = error ("Constructor application is not saturated: "++show (place (Con c ts) k))
drive (Con c ts) (CaseCtx k bs) fv s d = case (find (\(Branch c' xs t) -> c==c' && length xs == length ts) bs) of
                                            Nothing -> error ("No matching pattern in case for term:\n\n"++show (Case (Con c ts) bs))
                                            Just (Branch c' xs t) -> let fv' = foldr (\x fv -> let x' = renamevar fv x in x':fv) fv xs
                                                                         xs' = take (length xs) fv'
                                                                     in drive (rename (zip xs xs') t) k fv' ((zip xs' ts)++s) d
drive (App t u) k fv s d = drive t (AppCtx k u) fv s d
drive (Func f) k fv s d = case (lookup f d) of
                            Nothing -> error ("Undefined function: "++f)
                            Just t -> Unfold f (drive t k fv s d)
drive (Case t bs) k fv s d = drive t (CaseCtx k bs) fv s d
drive (Letrec f t u) k fv s d = let (fn,_) = unzip d
                                    f' = renamevar fn f
                                    t' = renameFun f f' t
                                    u' = renameFun f f' u
                                in  drive u' k fv s ((f',t'):d)
drive (Typed e t) k fv s d = Typed (drive e k fv s d) t
drive (Lit l) _ _ _ _ = Lit l

driveCtx t EmptyCtx fv s d = t
driveCtx t (AppCtx k u) fv s d = driveCtx (App t (drive u EmptyCtx fv s d)) k fv s d
driveCtx (Var x) (CaseCtx k bs) fv s d = Case (Var x) (map (\(Branch c xs t) -> let fv' = foldr (\x fv -> let x' = renamevar fv x in x':fv) fv xs
                                                                                    xs' = take (length xs) fv'
                                                                                in (Branch c xs' $ drive (rename (zip xs xs') t) k fv' ((x,Con c (map Var xs')):s) d)) bs)
driveCtx t (CaseCtx k bs) fv s d = (Case t (map (\(Branch c xs t) -> let fv' = foldr (\x fv -> let x' = renamevar fv x in x':fv) fv xs
                                                                         xs' = take (length xs) fv'
                                                                     in (Branch c xs' $ drive (rename (zip xs xs') t) k fv' s d)) bs))

distill (Var x) fv r = Var x
distill (Lambda x t) fv r = let x' = renamevar fv x
                            in Lambda x' (distill (rename [(x,x')] t) (x':fv) r)
distill (Con c ts) fv r = Con c (map (\t -> distill t fv r) ts)
distill (App t u) fv r = App (distill t fv r) (distill u fv r)
distill (Case t bs) fv r = Case (distill t fv r) (map (\(Branch c xs t) -> let fv' = foldr (\x fv -> let x' = renamevar fv x in x':fv) fv xs
                                                                               xs' = take (length xs) fv'
                                                                           in (Branch c xs' $ distill (rename (zip xs xs') t) fv' r)) bs)
distill (Let x t u) fv r = case (find (\(l,t') -> isJust (renaming t' (Let x t u) [] [])) r) of
                              Just (l,t') -> Label l (Let x t u)
                              Nothing -> case (find (\(l,t') -> isJust (couple t' (Let x t u) [] [])) r) of
                                            Just (l,t') -> let (t'',s) = generalise (Let x t u) t' [] [] fv []
                                                           in  distill (makeLet s  t'') fv r
                                            Nothing -> let l = renamevar (fst (unzip r)) "l"
                                                           x' = renamevar fv x
                                                           t' = rename [(x,x')] t
                                                           u' = rename [(x,x')] u
                                                           t'' = distill t' (x':fv) ((l,Let x' t' u'):r)
                                                           u'' = distill u' (x':fv) ((l,Let x' t' u'):r)
                                                       in Label l (Let x' t'' u'')
distill (Unfold f t) fv r = case (find (\(l,t') -> isJust (renaming t' (Unfold f t) [] [])) r) of
                               Just (l,t') -> Label l (Unfold f t)
                               Nothing -> case (find (\(l,t') -> isJust (couple t' (Unfold f t) [] [])) r) of
                                             Just (l,t') -> let (t'',s) = generalise (Unfold f t) t' [] [] fv []
                                                            in  distill (makeLet s  t'') fv r
                                             Nothing -> let l = renamevar (fst (unzip r)) "l"
                                                        in Label l (Unfold f (distill t fv ((l,Unfold f t):r)))
distill (Subst t u) fv r = Subst t (distill u fv r)
distill (Typed e t) fv r = Typed (distill e fv r) t
distill (Lit l) _ _ = Lit l

super (Var x) k fv r d = superCtx (Var x) k fv r d
super (Lambda x t) EmptyCtx fv r d = let x' = renamevar fv x
                                     in Lambda x' (super (rename [(x,x')] t) EmptyCtx (x':fv) r d)
super (Lambda x t) (AppCtx k t') fv r d = super (subst x t' t) k fv r d
super (Lambda x t) (CaseCtx k bs) fv r d = error "Unapplied function in case selector"
super (Con c ts) EmptyCtx fv r d = Con c (map (\t -> super t EmptyCtx fv r d) ts)
super (Con c ts) (AppCtx k t) fv r d = error ("Constructor application is not saturated: "++show (place (Con c ts) k))
super (Con c ts) (CaseCtx k bs) fv r d = case (find (\(Branch c' xs t) -> c==c' && length xs == length ts) bs) of
                                            Nothing -> error ("No matching pattern in case for term:\n\n"++show (Case (Con c ts) bs))
                                            Just (Branch c' xs t) -> super (foldr (\(x,t) u -> subst x t u) t (zip xs ts)) k fv r d
super (App t u) k fv r d = super t (AppCtx k u) fv r d
super (Func f) k fv r d = let t = place (Func f) k
                         in  case (find (\(l,t') -> isJust (renaming t' t [] [])) r) of
                                Just (l,t') -> Label l t
                                Nothing -> case (find (\(l,t') -> isJust (couple t' t [] [])) r) of
                                              Just (l,t') -> let (t'',s) = generalise t t' [] [] fv []
                                                             in super (makeLet s t'') EmptyCtx fv r d
                                              Nothing -> case (lookup f d) of
                                                            Nothing -> error ("Undefined function: "++f)
                                                            Just t' -> let l = renamevar (fst (unzip r)) "l"
                                                                       in Label l (super t' k fv ((l,t):r) d)
super (Case t bs) k fv r d = super t (CaseCtx k bs) fv r d
super (Let x t u) k fv r d = let x' = renamevar fv x
                             in Let x' (super t EmptyCtx fv r d) (super (rename [(x,x')] u) k (x':fv) r d)
super (Letrec f t u) k fv r d = super u k fv r ((f,t):d)
super (Typed e t) k fv r d = Typed (super e k fv r d) t

superCtx t EmptyCtx fv r d = t
superCtx t (AppCtx k u) fv r d = superCtx (App t (super u EmptyCtx fv r d)) k fv r d
superCtx (Var x) (CaseCtx k bs) fv r d = Case (Var x) (map (\(Branch c xs t) -> let fv' = foldr (\x fv -> let x' = renamevar fv x in x':fv) fv xs
                                                                                    xs' = take (length xs) fv'
                                                                                in (Branch c xs' $ super (subst x (Con c (map Var xs')) (rename (zip xs xs') t)) k fv' r d)) bs)
superCtx t (CaseCtx k bs) fv r d = (Case t (map (\(Branch c xs t) -> let fv' = foldr (\x fv -> let x' = renamevar fv x in x':fv) fv xs
                                                                         xs' = take (length xs) fv'
                                                                     in (Branch c xs' $ super (rename (zip xs xs') t) k fv' r d)) bs))
