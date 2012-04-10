module Core.Expr where

import qualified Language.Haskell.Syntax as L
import Data.List (intersect)
import Data.Foldable (find, foldrM)
import Data.Maybe (isJust)
import Control.Monad (mplus, msum)
import Control.Arrow (second)

data Program = Program Expr [Function]
    deriving Eq
    
data Expr = Var String
          | Bound Int
          | Con String [Expr]
          | Func String
          | Lambda String Expr
          | App Expr Expr
          | InfixApp Expr String Expr
          | Case Expr [Branch]
          | Let String Expr Expr
          | Lit L.HsLiteral
          | Typed Expr L.HsQualType
          | Unfold String Expr Expr
          | Fold String Expr
          
instance Eq Expr where
    (Var v)         == (Var v')             = v == v'
    (Bound b)       == (Bound b')           = b == b'
    (Con c es)      == (Con c' es')         = c == c' && es == es'
    (Func f)        == (Func f')            = f == f'
    (Lambda v e)    == (Lambda v' e')       = v == v' && e == e'
    (App e a)       == (App e' a')          = e == e' && a == a'
    (InfixApp e c f)== (InfixApp e' c' f')  = e == e' && c == c' && f == f'
    (Case e b)      == (Case e' b')         = e == e' && b == b'
    (Let v e f)     == (Let v' e' f')       = v == v' && e == e' && f == f'
    (Lit l)         == (Lit l')             = l == l'
    (Typed e t)     == (Typed e' t')        = e == e' && t == t'
    (Unfold s e f)  == (Unfold s' e' f')    = s == s' && e == e' && f == f'
    (Fold s e)      == (Fold s' e')         = s == s' && e == e'
    _ == _ = False
          
data Pattern = Pattern String [String]
    deriving Eq
    
type Branch = (Pattern, Expr)
type Function = (String, Expr)

renaming (Var x) (Var x') s 
 | x `elem` fst (unzip s) && (x, x') `elem` s = Just s
 | x `elem` fst (unzip s) = Nothing
 | otherwise = Just ((x, x'):s)
renaming (Bound i) (Bound i') s 
 | i == i' = Just s
renaming (Lambda _ t) (Lambda _ t') s = renaming t t' s
renaming (Con c ts) (Con c' ts') s 
 | c == c' && length ts == length ts' = foldrM (\(t,t') s -> renaming t t' s) s (zip ts ts')
renaming (App t u) (App t' u') s = renaming t t' s >>= renaming u u'
renaming (InfixApp t c u) (InfixApp t' c' u') s
 | c == c'  = renaming t t' s >>= renaming u u'
renaming (Func f) (Func f') s | f==f' = Just s
renaming (Case t bs) (Case t' bs') s 
 | match bs bs' = renaming t t' s >>= \s -> foldrM (\((_,t),(_,t')) s -> renaming t t' s) s (zip bs bs')
renaming (Let _ t u) (Let _ t' u') s = renaming t t' s >>= renaming u u'
renaming (Unfold f t u) (Unfold f' t' u') s 
 | f==f' = renaming u u' s
renaming (Fold f t) (Fold f' t') s 
 | f==f' = Just s
renaming (Typed e _) (Typed e' _) s = renaming e e' s
renaming t t' s = Nothing

inst (Var x) t s 
 | x `elem` fst (unzip s) && (x, t) `elem` s = Just s
 | x `elem` fst (unzip s) = Nothing
 | otherwise = Just ((x, t):s)
inst (Bound i) (Bound i') s 
 | i == i' = Just s
inst (Lambda _ t) (Lambda _ t') s = inst t t' s
inst (Con c ts) (Con c' ts') s 
 | c == c' && length ts == length ts' = foldrM (\ (t,t') s -> inst t t' s) s (zip ts ts')
inst (App t u) (App t' u') s = inst t t' s >>= inst u u'
inst (InfixApp e c f) (InfixApp e' c' f') s
 | c == c' = inst e e' s >>= inst f f'
inst (Func f) (Func f') s 
 | f == f' = Just s
inst (Case t bs) (Case t' bs') s 
 | match bs bs' = inst t t' s >>= \s -> foldrM (\ ((_,t),(_,t')) s -> inst t t' s) s (zip bs bs')
inst (Let _ t u) (Let _ t' u') s = inst t t' s >>= inst u u'
inst (Unfold f t u) (Unfold f' t' u') s 
 | f==f' = inst u u' s
inst (Fold f t) (Fold f' t') s 
 | f==f' = Just s
inst (Typed e _) (Typed e' _) s = inst e e' s
inst t t' s = Nothing

isRenaming [] = True
isRenaming ((x,Var x'):s) = isRenaming s
isRenaming s = False

embedding t u s = mplus (couple t u s) (dive t u s)

couple (Var x) (Var x') s
 | x `elem` fst (unzip s) && (x,x') `elem` s = Just s
 | x `elem` fst (unzip s) = Nothing
 | otherwise = Just ((x,x'):s)
couple (Bound i) (Bound i') s 
 | i == i' = Just s
couple (Lambda _ t) (Lambda _' t') s = embedding t t' s
couple (Con c' ts) (Con c'' ts') s 
 | c' == c'' && length ts == length ts' = foldrM (\ (t,t') s -> embedding t t' s) s (zip ts ts')
couple (App t u) (App t' u') s = embedding t t' s >>= embedding u u'
couple (InfixApp e c f) (InfixApp e' c' f') s
 | c == c' = embedding e e' s >>= embedding f f'
couple (Func f) (Func f') s 
 | f==f' = Just s
couple (Case t bs) (Case t' bs') s 
 | match bs bs' = embedding t t' s >>= \s -> foldrM (\ ((_,t),(_,t')) s -> embedding t t' s) s (zip bs bs')
couple (Let _ t u) (Let _ t' u') s = embedding t t' s >>= embedding u u'
couple (Unfold f t u) (Unfold f' t' u') s 
 | f==f' = embedding u u' s
couple (Fold f t) (Fold f' t') s 
 | f==f' = Just s
couple (Typed e _) (Typed e' _) s = couple e e' s
couple t u s = Nothing

dive t (Con _ ts) s = msum (map (\t' -> embedding t t' s) ts)
dive t (App t' u) s = mplus (embedding t t' s) (embedding t u s)
dive t (InfixApp e _ f) s = mplus (embedding t e s) (embedding t f s)
dive t (Case t' bs) s = mplus (embedding t t' s) (msum (map (\(Pattern _ vs,t') -> embedding t (shift (length vs) 0 t') s) bs))
dive t (Unfold f _ t') s = embedding t t' s
dive t u s = Nothing

generalise (Var x) (Var x') s fv bv = (Var x,s)
generalise (Bound i) (Bound i') s fv bv 
 | i==i' = (Bound i,s)
generalise (Lambda x t) (Lambda x' t') s fv bv = 
    let 
        (gv,_) = unzip s
        x'' = renamevar (gv ++ fv) x
        (t'',s') = generalise (subst 0 (Var x'') t) (subst 0 (Var x'') t') s (x'':fv) (x'':bv)
    in (Lambda x (abstract 0 x'' t''),s')
generalise (Con c ts) (Con c' ts') s fv bv 
 | c==c' && length ts == length ts' = 
     let (ts'',s') = foldr (\(t,t') (ts,s) -> let (t'',s') = generalise t t' s fv bv
                                              in (t'':ts,s')) ([],s) (zip ts ts')
     in (Con c ts'',s')
generalise (App t u) (App t' u') s fv bv = 
    let 
        (t'',s') = generalise t t' s fv bv
        (u'',s'') = generalise u u' s' fv bv
    in (App t'' u'',s'')
generalise (InfixApp e c f) (InfixApp e' c' f') s fv bv
 | c == c' =
     let
        (e'', s') = generalise e e' s fv bv
        (f'', s'') = generalise f f' s' fv bv
     in (InfixApp e'' c f'', s'')
generalise (Func f) (Func f') s fv bv 
 | f==f' = (Func f,s)
generalise (Case t bs) (Case t' bs') s fv bv 
 | match bs bs' = 
     let 
        (t'',s') = generalise t t' s fv bv
        (bs'',s'') = foldr (\ ((Pattern c xs,t),(Pattern c' xs',u)) (bs,s) -> let (gv,_) = unzip s
                                                                                  fv' = foldr (\x fv -> let x'=renamevar (gv++fv) x in x':fv) fv xs
                                                                                  xs'' = take (length xs) fv'
                                                                                  t' = foldr (\x t -> subst 0 (Var x) t) t xs''
                                                                                  u' = foldr (\x t -> subst 0 (Var x) t) u xs''
                                                                                  (t'',s') = generalise t' u' s fv' (xs''++bv)
                                                                              in ((Pattern c' xs',foldl (flip (abstract 0)) t'' xs''):bs,s')) ([],s') (zip bs bs')
    in (Case t'' bs'',s'')
generalise (Let x t u) (Let x' t' u') s fv bv = 
    let 
        x'' = renamevar (fst (unzip s)++fv) x
        (t'',s') = generalise t t' s fv bv
        (u'',s'') = generalise (subst 0 (Var x'') u) (subst 0 (Var x'') u') s' (x'':fv) (x'':bv)
    in (subst 0 t'' (abstract 0 x'' u''),s'')
generalise (Unfold f t u) (Unfold f' t' u') s fv bv 
 | f==f' = let 
                (u'',s') = generalise u u' s fv bv
           in (Unfold f t u'',s')
generalise (Fold f t) (Fold f' t') s fv bv 
 | f==f' = (Fold f t,s)
generalise (Typed e t) (Typed e' t') s fv bv
 | t == t' = 
     let
        (e'', s') = generalise e e' s fv bv
     in (Typed e'' t, s')
generalise t t' s fv bv = 
    let 
        xs = intersect (free t) bv
        t'' = foldr (\x t -> Lambda x (abstract 0 x t)) t xs
        (gv,_) = unzip s
        x = renamevar (gv++fv) "x"
    in (foldr (flip App . Var) (Var x) xs, (x, t''):s)

extract s t = foldr (\(x,t) u -> Let x t (abstract 0 x u)) t s

residualise (Var x) fv r d = (Var x,d)
residualise (Bound i) fv r d = (Bound i,d)
residualise (Lambda x t) fv r d = let x' = renamevar fv x
                                      (t',d') = residualise (subst 0 (Var x') t) (x':fv) r d
                                  in  (Lambda x (abstract 0 x' t'),d')
residualise (Con c ts) fv r d = let (ts',d') = foldr (\t (ts,d) -> let (t',d') = residualise t fv r d
                                                                    in (t':ts,d')) ([],d) ts
                                 in (Con c ts',d')
residualise (App t u) fv r d = let (t',d') = residualise t fv r d
                                   (u',d'') = residualise u fv r d'
                               in  (App t' u',d'')
residualise (InfixApp e c f) fv r d =
    let
        (e', d') = residualise e fv r d
        (f', d'') = residualise f fv r d'
    in (InfixApp e' c f', d'')
residualise (Func f) fv r d = error ("Function not unfolded: " ++ f)
residualise (Case t bs) fv r d = let (t',d') = residualise t fv r d
                                     (bs',d'') = foldr (\(Pattern c xs,t) (bs,d) -> let fv' = foldr (\x fv -> let x'=renamevar fv x in x':fv) fv xs
                                                                                        xs' = take (length xs) fv'
                                                                                        (t',d') = residualise (foldr (\x t -> subst 0 (Var x) t) t xs') fv' r d
                                                                                    in  ((Pattern c xs,foldl (flip (abstract 0)) t' xs'):bs,d')) ([],d') bs
                                 in  (Case t' bs',d'')
residualise (Let x t u) fv r d = let x' = renamevar fv x
                                     (t',d') = residualise t fv r d
                                     (u',d'') = residualise (subst 0 (Var x') u) (x':fv) r d'
                                 in  (subst 0 t' (abstract 0 x' u'),d'')
residualise (Unfold f t u) fv r d = let (fs,_) = unzip d
                                        (fs',_) = unzip r
                                        f' = renamevar (fs++fs') "f"
                                        (u',d') = residualise u fv ((f',t):r) d
                                    in  if f' `elem` funs [] u'
                                        then let xs = free t
                                             in  (foldr (\x t -> App t (Var x)) (Func f') xs,(f',foldl (\t x -> Lambda x (abstract 0 x t)) u' xs):d')
                                        else (u',d')
residualise (Fold f t) fv r d = case find (\(f',t') -> isJust (renaming t' t [])) r of
    Just (f',t') -> (foldr (\x t -> App t (Var x)) (Func f') (free t), d)
    _ -> error ("Fold has no matching unfold: " ++ f)
residualise (Typed e t) fv r d = 
    let
        (e', d') = residualise e fv r d
    in (Typed e' t, d')
residualise e@(Lit l) _ _ d = (e, d)

match bs bs' = (length bs == length bs') && all (\((Pattern c xs,t),(Pattern c' xs',t')) -> c == c' && length xs == length xs') (zip bs bs')

free = free' []

free' xs (Var x) 
 | x `elem` xs = xs
 | otherwise = x:xs
free' xs (Bound _) = xs
free' xs (Lambda _ t) = free' xs t
free' xs (Con _ ts) = foldr (flip free') xs ts
free' xs (App t u) = free' (free' xs t) u
free' xs (InfixApp e _ e') = free' (free' xs e) e'
free' xs (Func _) = xs
free' xs (Case t bs) = foldr (\(_,t) xs' -> free' xs' t) (free' xs t) bs
free' xs (Let _ t u) = free' (free' xs t) u
free' xs (Unfold f t u) = free' xs u
free' xs (Fold f t) = xs
free' xs (Typed e _) = free' xs e
free' xs _ = xs

bound = bound' 0 []

bound' d bs (Var _) = bs
bound' d bs (Bound i) 
 | i >= d && (i - d) `elem` bs = bs
 | i >= d = (i - d) : bs
 | otherwise = bs
bound' d bs (Lambda _ t) = bound' (d+1) bs t
bound' d bs (Con _ ts) = foldr (flip (bound' d)) bs ts
bound' d bs (App t u) = bound' d (bound' d bs u) t
bound' d bs (InfixApp e _ e') = bound' d (bound' d bs e') e
bound' d bs (Func _) = bs
bound' d bs (Case t bs') = foldr (\(Pattern _ xs,t) bs -> bound' (d+length xs) bs t) (bound' d bs t) bs'
bound' d bs (Let _ t u) = bound' d (bound' (d+1) bs u) t
bound' d bs (Unfold f t u) = bound' d bs u
bound' d bs (Fold f t) = bs
bound' d bs (Typed e _) = bound' d bs e

funs fs (Var x) = fs
funs fs (Bound i) = fs
funs fs (Lambda x t) = funs fs t
funs fs (Con c ts) = foldr (flip funs) fs ts
funs fs (Func f) = f:fs
funs fs (App t u) = funs (funs fs t)  u
funs fs (InfixApp e _ e') = funs (funs fs e) e'
funs fs (Case t bs) = foldr (\(_, t) fs -> funs fs t) (funs fs t) bs
funs fs (Let x t u) = funs (funs fs t) u
funs fs (Unfold f t u) = fs
funs fs (Fold f t) = fs
funs fs (Typed e _) = funs fs e
funs fs (Lit _) = fs

unfold fs (Var x) = Var x
unfold fs (Bound i) = Bound i
unfold fs (Lambda x t) = Lambda x (unfold fs t)
unfold fs (Con c ts) = Con c (map (unfold fs) ts)
unfold fs (Func f) = case lookup f fs of
    Just t  -> Unfold f (Func f) (unfold fs t)
    _ -> error ("Undefined function: "++f)
unfold fs (App t u) = App (unfold fs t) (unfold fs u)
unfold fs (InfixApp e c e') = InfixApp (unfold fs e) c (unfold fs e')
unfold fs (Case t bs) = Case (unfold fs t) (map (second (unfold fs)) bs)
unfold fs (Let x t u) = Let x (unfold fs t) (unfold fs u)
unfold fs (Unfold f t u) = Unfold f t u
unfold fs (Fold f t) = Fold f t
unfold fs (Typed e t) = Typed (unfold fs e) t


shift 0 d u = u
shift i d e@(Var _) = e
shift i d e@(Bound j)
 | j >= d = Bound (j + 1)
 | otherwise = e
shift i d (Lambda x t) = Lambda x (shift i (d+1) t)
shift i d (Con c ts) = Con c (map (shift i d) ts)
shift i d (App t u) = App (shift i d t) (shift i d u)
shift i d (InfixApp e c e') = InfixApp (shift i d e) c (shift i d e')
shift i d (Func f) = Func f
shift i d (Case t bs) = Case (shift i d t) (map (\(Pattern c xs,t) -> (Pattern c xs,shift i (d+length xs) t)) bs)
shift i d (Let x t u) = Let x (shift i d t) (shift i (d+1) u)
shift i d (Unfold f t u) = Unfold f (shift i d t) (shift i d u)
shift i d (Fold f t) = Fold f (shift i d t)
shift i d (Typed e t) = Typed (shift i d e) t
shift _ _ e@(Lit _) = e

subst i t (Var x) = Var x
subst i t (Bound i') 
 | i' < i = Bound i'
 | i == i' = shift i 0 t
 | otherwise = Bound (i' - 1)
subst i t (Lambda x t') = Lambda x (subst (i+1) t t')
subst i t (Con c ts) = Con c (map (subst i t) ts)
subst i t (App t' u) = App (subst i t t') (subst i t u)
subst i t (InfixApp e c e') = InfixApp (subst i t e) c (subst i t e')
subst i t (Func f) = Func f
subst i t (Case t' bs) = Case (subst i t t') (map (\(Pattern c xs,u) -> (Pattern c xs,subst (i+length xs) t u)) bs)
subst i t (Let x t' u) = Let x (subst i t t') (subst (i+1) t u)
subst i t (Unfold f t' u) = Unfold f (subst i t t') (subst i t u)
subst i t (Fold f t') = Fold f (subst i t t')
subst i t (Typed e t') = Typed (subst i t e) t'
subst _ _ e@(Lit _) = e

abstract i b e@(Var x)
 | x == b = Bound i
 | otherwise = e
abstract i b e@(Bound i')
 | i' >= i = Bound (i' + 1)
 | otherwise = e
abstract i b (Lambda x t) = Lambda x (abstract (i+1) b t)
abstract i b (Con c ts) = Con c (map (abstract i b) ts)
abstract i b (App t u) = App (abstract i b t) (abstract i b u)
abstract i b (InfixApp e c e') = InfixApp (abstract i b e) c (abstract i b e')
abstract i b (Func f) = Func f
abstract i b (Case t bs) = Case (abstract i b t) (map (\(Pattern c xs,t) -> (Pattern c xs,abstract (i + length xs) b t)) bs)
abstract i b (Let x t u) = Let x (abstract i b t) (abstract (i+1) b u)
abstract i b (Unfold f t u) = Unfold f (abstract i b t) (abstract i b u)
abstract i b (Fold f t) = Fold f (abstract i b t)
abstract i b (Typed e t) = Typed (abstract i b e) t
abstract i b e = e

rename s e@(Var x) = case lookup x s of
    Just x'  -> Var x'
    _ -> e
rename s (Bound i) = Bound i
rename s (Lambda a t) = Lambda a (rename s t)
rename s (Con c ts) = Con c (map (rename s) ts)
rename s (App t u) = App (rename s t) (rename s u)
rename s (InfixApp e c e') = InfixApp (rename s e) c (rename s e')
rename s (Func f) = Func f
rename s (Case t bs) = Case (rename s t) (map (second (rename s)) bs)
rename s (Let x t u) = Let x (rename s t) (rename s u)
rename s (Unfold f t u) = Unfold f (rename s t) (rename s u)
rename s (Fold f t) = Fold f (rename s t)
rename s (Typed e t) = Typed (rename s e) t

replace t u v
 | t == v = u
 | otherwise = replace' t u v
 
replace' t u (Var x) = Var x
replace' t u (Bound i) = Bound i
replace' t u (Lambda x t') = Lambda x (replace (shift 1 0 t) (shift 1 0 u) t')
replace' t u (Con c ts) = Con c (map (replace t u) ts)
replace' t u (App t' u') = App (replace t u t') (replace t u u')
replace' t u (InfixApp e c e')= InfixApp (replace t u e) c (replace t u e')
replace' t u (Func f) = Func f
replace' t u (Case t' bs) = Case (replace t u t') (map (\(Pattern c xs,t') -> (Pattern c xs,replace (shift (length xs) 0 t) (shift (length xs) 0 u) t')) bs)
replace' t u (Let x t' u') = Let x (replace' t u t') (replace' (shift 1 0 t) (shift 1 0 u) u')
replace' t u (Unfold f t' u') = Unfold f (replace' t u t') (replace' t u u')
replace' t u (Fold f t') = Fold f (replace' t u t')
replace' t u (Typed e t') = Typed (replace t u e) t'

renamevar xs x
 | x `elem` xs = renamevar xs (x ++ "'")
 | otherwise = x

stripLambda (Lambda x t) = 
    let 
        x' = renamevar (free t) x
        (xs,u) = stripLambda (subst 0 (Var x') t)
    in (x':xs,u)
stripLambda t = ([],t)

isList (Con "Nil" []) = True
isList (Con "Con" [h,t]) = isList t
isList _ = False

list2con [] = Con "Nil" []
list2con (h:t) = Con "Con" [h,list2con t]

con2list (Con "Nil" [])  = []
con2list (Con "Con" [h,t]) = h:con2list t

isNat (Con "Zero" []) = True
isNat (Con "Succ" [n]) = isNat n
isNat _ = False

nat2con 0 = Con "Zero" []
nat2con n = Con "Succ" [nat2con (n-1)]

con2nat (Con "Zero" [])  = 0
con2nat (Con "Succ" [n]) = 1+con2nat n