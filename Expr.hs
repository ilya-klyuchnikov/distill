module Expr where

import Data.Maybe (isJust)
import Data.List (intersect) 
import Data.Foldable (foldrM,find)
import Debug.Trace
import Control.Monad (msum, mplus)
import Text.PrettyPrint.HughesPJ
import Language.Haskell.Pretty (prettyPrint)
import qualified Language.Haskell.Syntax as L

class Pretty a where
    pretty :: a -> Doc
    prettyShow :: a -> String
    prettyShow = render . pretty
    
class Matchable a where
    match :: a -> a -> Bool

data Expr = Var String
          | Bound Int
          | Lambda String Expr
          | Con String [Expr]
          | App Expr Expr
          | Func String
          | Case Expr [Branch]
          | Let String Expr Expr
          | Unfold String Expr Expr
          | Fold String Expr
          | Lit L.HsLiteral
          | Typed Expr L.HsQualType

data Branch = Branch String [String] Expr

data Program = Program [L.HsImportDecl] [L.HsDecl] L.HsDecl Expr [(String,Expr)]

instance Show Expr where
    show = prettyShow
    
instance Show Program where
    show = prettyShow

instance Show Branch where
    show = prettyShow

instance Eq Expr where
   (==) (Var v) (Var v') = v == v'
   (==) (Bound i) (Bound i') = i == i'
   (==) (Lambda _ e) (Lambda _ e') = e == e'
   (==) (Con c es) (Con c' es') = c == c' && es == es'
   (==) (App e f) (App e' f') = e == e' && f == f'
   (==) (Func f) (Func f') = f == f'
   (==) c@(Case e bs) c'@(Case e' bs') = match c c' && all (\ (Branch _ _ e, Branch _ _ e') -> e == e') (zip bs bs')
   (==) (Let _ e f) (Let _ e' f') = e == e' && f == f'
   (==) (Unfold _ e f) (Unfold _ e' f') = e == e' && f == f'
   (==) (Fold f _) (Fold f' _) = f == f'
   (==) (Lit l) (Lit l') = l == l'
   (==) (Typed e t) (Typed e' t') = e == e' && t == t'
   (==) t t' = False
   
instance Matchable Expr where
    match (Con c ts) (Con c' ts') = c == c' && length ts == length ts'
    match (Case _ bs) (Case _ bs') = length bs == length bs' && all (uncurry match) (zip bs bs')
    match (Unfold f _ _) (Unfold f' _ _) = f == f'
    match (Typed _ t) (Typed _ t') = t == t'
    match e e' = e == e'
   
instance Matchable Branch where
    match (Branch c xs t) (Branch c' xs' t') = c == c' && length xs == length xs'
  
instance Pretty Program where
   pretty (Program imports dataDecls main root funcs) = vcat $ (punctuate (text "\n") $ (text ("module Main(main) where\n")):(map (text . prettyPrint) imports) ++ (map (text . prettyPrint) dataDecls) ++ [text $ prettyPrint main] ++ (map prettyFunction (("root", root):funcs)))

prettyCons (Con "Nil" []) = text "[]"
prettyCons (Con "Cons" ((Con "Nil" []):[])) = text "[]"
prettyCons (Con "Cons" (x:Con "Nil" []:[])) = brackets $ pretty x
prettyCons (Con "Cons" (x:[])) = parens (pretty x)
prettyCons (Con "Cons" (x:xs)) = parens (pretty x <> colon <> pretty (Con "Cons" xs))

instance Pretty Expr where
   pretty (Var v)
    | v `elem` [".", ":", "$", "$$", "==", "<=", ">=", "<", ">", "=<<", "+", "-", "/", "*", "!!", "/=", "%", "++"] = parens $ text v
    | otherwise = text v
   pretty (Bound i) = text "#" <> int i
   pretty (Con "Nil" []) = text "[]"
   pretty (Con "Cons" ((Con "Nil" []):[])) = text "[]"
   pretty (Con "Cons" (x:xs)) = parens (pretty x <> colon <> prettyCons (Con "Cons" xs))
   pretty con@(Con c es)
    | isList con = brackets $ hcat (punctuate comma (map pretty $ con2list con))
    | c == "Nil" = text "Nil"
    | c == "Cons" && length es == 2 = text "Cons" <+> (parens $ pretty $ es !! 0) <+> (parens $ pretty $ es !! 1) -- hcat $ punctuate colon $ map pretty es
    | isNat con = int $ con2nat con
    | otherwise = text c <+> hcat (punctuate space $ map (parens . pretty) es)
   pretty (Lit l) = text $ prettyPrint l
   pretty (Func f) = text f
   pretty (App e@(Lambda {}) e') = parens (pretty e) <+> pretty e'
   pretty (App (App (Var "Cons") e) e') = parens $ pretty e <> colon <> pretty e'
   pretty (App (App (Var v) e) e')
    | v `elem` [".", ":", "$", "$$", "==", "<=", ">=", "<", ">", "=<<", "+", "-", "/", "*", "!!", "/=", "%", "++"] = parens $ hcat $ punctuate (space <> text v <> space) (map pretty [e, e'])
   pretty (App e e') = pretty e <+> (parens $ pretty e')
   pretty (Case e b) = parens $ hang (text "case" <+> pretty e <+> text "of") 1 $ vcat $ map pretty b
   pretty e'@(Lambda v e) = let (vs, f) = stripLambda e'
                            in parens $ text "\\" <> hsep (map (parens . text) vs) <+> text "->" <+> pretty f
   pretty (Typed e t) = parens (parens (pretty e) <+> text "::" <> text (prettyPrint t))


instance Pretty Branch where
   pretty (Branch c [] e)
    | c == "Nil" = text "[]" <+> text "->" <+> pretty e
    | otherwise = text c <+> text "->" <+> pretty e
   pretty (Branch c vs e) = let vs' = map (renamevar (free e)) vs
                                e' = foldr (\v e -> subst 0 (Var v) e) e vs'
                            in (if c == "Nil"
                                then (text "[]")
                                else if c == "Cons"
                                      then pretty (Con c (map Var vs))
                                      else text c <+> (hcat (punctuate space (map (parens . text) vs')))) <+> text "->" <+> pretty e' $$ empty
                            
prettyFunction :: (String, Expr) -> Doc
prettyFunction (name, body) = text name <+> text "=" <+> pretty body

renaming (Var v) (Var v') s 
 | (v, v') `elem` s = Just s
 | v `elem` fst (unzip s) = Nothing
 | otherwise = Just ((v, v'):s)
renaming (Bound i) (Bound i') s | i == i' = Just s
renaming (Lambda _ t) (Lambda _ t') s = renaming t t' s
renaming (Con c ts) (Con c' ts') s | c == c' && length ts == length ts' = foldrM (\(t,t') s -> renaming t t' s) s (zip ts ts')
renaming (App t u) (App t' u') s = (renaming t t' s) >>= (renaming u u')
renaming (Func f) (Func f') s | f==f' = Just s
renaming c@(Case t bs) c'@(Case t' bs') s | match c c' = (renaming t t' s) >>= (\s -> foldrM (\((Branch _ _ t),(Branch _ _ t')) s -> renaming t t' s) s (zip bs bs'))
renaming (Let _ t u) (Let _ t' u') s = (renaming t t' s) >>= (renaming u u')
renaming (Unfold f t u) (Unfold f' t' u') s | f==f' = renaming u u' s
renaming (Fold f t) (Fold f' t') s | f==f' = Just s
renaming (Typed e t) (Typed e' t') s | t == t' = renaming e e' s
renaming t t' s = Nothing


inst (Var v) e s 
 | (v, e) `elem` s = Just s
 | v `elem` fst (unzip s) = Nothing
 | otherwise = Just ((v, e):s)
inst (Bound i) (Bound i') s 
 | i == i' = Just s
inst (Lambda _ t) (Lambda _ t') s = inst t t' s
inst (Con c ts) (Con c' ts') s 
 | c == c' && length ts == length ts' = foldrM (\ (t,t') s -> inst t t' s) s (zip ts ts')
inst (App t u) (App t' u') s = (inst t t' s) >>= (inst u u')
inst (Func f) (Func f') s 
 | f == f' = Just s
inst c@(Case t bs) c'@(Case t' bs') s 
 | match c c' = (inst t t' s) >>= (\s->foldrM (\ ((Branch _ _ t),(Branch _ _ t')) s -> inst t t' s) s (zip bs bs'))
inst (Let _ t u) (Let _ t' u') s = (inst t t' s) >>= (inst u u')
inst (Unfold f t u) (Unfold f' t' u') s 
 | f==f' = inst u u' s
inst (Fold f t) (Fold f' t') s 
 | f==f' = Just s
inst (Typed e t) (Typed e' t') s
 | t == t' = inst e e' s
inst _ _ _ = Nothing

isRenaming [] = True 	
isRenaming ((_,Var _):s) = isRenaming s
isRenaming s = False

embedding t u s = mplus (couple t u s) (dive t u s)

couple (Var x) (Var x') s = if x `elem` fst (unzip s)
                                then if (x,x') `elem` s then Just s else Nothing
                                else Just ((x,x'):s)
couple (Bound i) (Bound i') s | i == i' = Just s
couple (Lambda _ t) (Lambda _' t') s = embedding t t' s
couple (Con c' ts) (Con c'' ts') s | c' == c'' && length ts == length ts' = foldrM (\ (t,t') s -> embedding t t' s) s (zip ts ts')
couple (App t u) (App t' u') s = (couple t t' s) >>= (embedding u u')
couple (Func f) (Func f') s | f==f' = Just s
couple c@(Case t bs) c'@(Case t' bs') s | match c c' = (embedding t t' s) >>= (\s->foldrM (\ ((Branch _ _ t),(Branch _ _ t')) s -> embedding t t' s) s (zip bs bs'))
couple (Let _ t u) (Let _ t' u') s = (embedding t t' s) >>= (embedding u u')
couple (Unfold f t u) (Unfold f' t' u') s | f==f' = embedding u u' s
couple (Fold f t) (Fold f' t') s | f==f' = Just s
couple (Typed e t) (Typed e' t') s | t == t' = embedding e e' s
couple (Lit l) (Lit l') s | l == l' = Just s
couple t u s = Nothing

dive e (Con _ es) s = msum (map (\e' -> embedding e e' s) es)
dive e (App e' f) s = mplus (embedding e e' s) (embedding e f s)
dive e (Case e' bs) s = mplus (embedding e e' s) (msum (map (\(Branch _ vs e') -> embedding e (shift (length vs) 0 e') s) bs))
dive e (Unfold _ _ e') s = embedding e e' s
dive e (Typed e' _) s = embedding e e' s
dive _ _ _ = Nothing

generalise e@(Var _) _ s _ _ = (e, s)
generalise f@(Lambda v e) (Lambda _ e') s fv bv = 
    let (gv,_) = unzip s
        v'' = renamevar (gv ++ fv) v
        (e'',s') = generalise (subst 0 (Var v'') e) (subst 0 (Var v'') e') s (v'':fv) (v'':bv)
    in (Lambda v (abstract 0 v'' e''), s') -- TODO: should this be Lambda v' ... ?
generalise e@(Con c es) e'@(Con _ es') s fv bv 
 | match e e' = 
     let (es'',s') = foldr (\(e, e') (es, s) -> let (e'', s') = generalise e e' s fv bv
                                                in (e'':es, s')) ([], s) (zip es es')
     in (Con c es'', s')
generalise (App e f) (App e' f') s fv bv 
 | isJust (couple e e' []) = 
     let (e'', s') = generalise e e' s fv bv
         (f'', s'') = generalise f f' s' fv bv
     in (App e'' f'', s'')
generalise e@(Case ce bs) e'@(Case ce' bs') s fv bv 
 | match e e' = 
     let (ce'', s') = generalise ce ce' s fv bv
         (bs'', s'') = foldr (\(Branch c vs e, Branch c' vs' f) (bs, s) -> let (gv, _) = unzip s
                                                                               fv' = foldr (\v fv -> (renamevar (gv ++ fv) v:fv)) fv vs
                                                                               vs'' = take (length vs) fv'
                                                                               e' = foldr (\v e -> subst 0 (Var v) e) e vs''
                                                                               f' = foldr (\v e -> subst 0 (Var v) e) f vs''
                                                                               (e'', s') = generalise e' f' s fv' (vs'' ++ bv)
                                                                           in (Branch c' vs' (foldl (flip (abstract 0)) e'' vs''):bs, s')) ([], s') (zip bs bs')
     in (Case ce'' bs'', s'')
generalise (Let v e f) (Let v' e' f') s fv bv = 
    let v'' = renamevar (fst (unzip s) ++ fv) v
        (e'', s') = generalise e e' s fv bv
        (f'', s'') = generalise (subst 0 (Var v'') f) (subst 0 (Var v'') f') s' (v'':fv) (v'':bv)
    in (Let v e'' (abstract 0 v'' f''), s'')
generalise e@(Unfold v e'' f) e'@(Unfold _ _ f') s fv bv 
 | match e e' = 
     let (f'', s') = generalise f f' s fv bv
     in (Unfold v e'' f'', s')
generalise e@(Typed f t) e'@(Typed f' _) s fv bv
 | match e e' = let (e'', s') = generalise f f' s fv bv
                in (Typed e'' t, s)
generalise e e' s fv bv 
 | match e e' = (e, s)
 | otherwise = 
     let vs = intersect (free e) bv
         e'' = foldr (\v e -> Lambda v (abstract 0 v e)) e vs
         (gv, _) = unzip s
         v = renamevar (gv ++ fv) "x"
     in (foldr (flip App . Var) (Var v) vs, (v, e''):s)

extract s s' e = foldr (\(v, e) f -> case e of
                                       Var v' -> subst 0 (Var v') (abstract 0 v f)
                                       _       -> case find (\ (v', e') -> e == e') s' of
                                                     Just (v', e') -> subst 0 (Var v') (abstract 0 v f)
                                                     Nothing -> Let v e (abstract 0 v f)) e s

residualise (Lambda v e) fv r d = 
    let v' = renamevar fv v
        (e', d') = residualise (subst 0 (Var v') e) (v':fv) r d
    in  (Lambda v (abstract 0 v' e'), d')
residualise (Con c es) fv r d = 
    let (es', d') = foldr (\e (es, d) -> let (e',d') = residualise e fv r d
                                         in (e':es, d')) ([], d) es
    in (Con c es', d')
residualise (App e f) fv r d = 
    let (e', d') = residualise e fv r d
        (f', d'') = residualise f fv r d'
    in  (App e' f', d'')
residualise (Func f) _ _ _ = error ("Funcction not unfolded: " ++ f)
residualise (Case e bs) fv r d = 
    let (e', d') = residualise e fv r d
        (bs', d'') = foldr (\(Branch c vs e) (bs, d) -> let fv' = foldr (\x fv -> (renamevar fv x:fv)) fv vs
                                                            vs' = take (length vs) fv'
                                                            (e',d') = residualise (foldr (\v e -> subst 0 (Var v) e) e vs') fv' r d
                                                        in  (Branch c vs (foldl (flip (abstract 0)) e' vs'):bs, d')) ([], d') bs
    in  (Case e' bs', d'')
residualise (Let v e f) fv r d = 
    let v' = renamevar fv v
        (e', d') = residualise e fv r d
        (f', d'') = residualise (subst 0 (Var v') f) (v':fv) r d'
    in  (subst 0 e' (abstract 0 v' f'),d'')
residualise (Unfold v e f) fv r d = 
    let (fs, _) = unzip d
        (fs', _) = unzip r
        v' = renamevar (fs ++ fs') "f"
        (f', d') = residualise f fv ((v', e):r) d
    in  if v' `elem` funs [] f'
         then let vs = free e
              in  (foldr (\v e -> App e (Var v)) (Func v') vs, (v', foldl (\e v -> Lambda v (abstract 0 v e)) f' vs):d')
         else (f', d')
residualise (Fold f e) fv r d = case find (\(f', e') -> isJust (renaming e' e [])) r of
                                   Just (f', e') -> let vs = free e
                                                   in (foldr (\v e -> App e (Var v)) (Func f') vs, d)
                                   Nothing -> error ("Fold has no matching unfold: " ++ f)
residualise (Typed f t) fv r d =
    let (f', d') = residualise f fv r d
    in (Typed f' t, d')
residualise e _ _ d = (e, d)

free t = free' [] t

free' vs (Var v)
 | v `elem` vs = vs
 | otherwise = v:vs
free' vs (Lambda _ e) = free' vs e
free' vs (Con _ es) = foldr (flip free') vs es
free' vs (App e f) = free' (free' vs e) f
free' vs (Case e bs) = foldr (\(Branch _ _ e) vs' -> free' vs' e) (free' vs e) bs
free' vs (Let _ e f) = free' (free' vs e) f
free' vs (Unfold _ _ f) = free' vs f
free' vs (Typed e _) = free' vs e
free' vs _ = vs

bound t = bound' 0 [] t

bound' d bs (Bound i)
 | i >= d && (i - d) `elem` bs = bs
 | i >= d = (i - d):bs
 | otherwise = bs
bound' d bs (Lambda _ e) = bound' (d + 1) bs e
bound' d bs (Con _ es) = foldr (flip (bound' d)) bs es
bound' d bs (App e f) = bound' d (bound' d bs f) e
bound' d bs (Case e bs') = foldr (\(Branch _ vs e) bs -> bound' (d + length vs) bs e) (bound' d bs e) bs'
bound' d bs (Let _ e f) = bound' d (bound' (d + 1) bs f) e
bound' d bs (Unfold _ _ f) = bound' d bs f
bound' d bs (Typed e _) = bound' d bs e
bound' _ bs _ = bs

funs fs (Lambda _ e) = funs fs e
funs fs (Con _ es) = foldr (flip funs) fs es
funs fs (Func f) = f:fs
funs fs (App e f) = funs (funs fs e) f
funs fs (Case e bs) = foldr (\(Branch _ _ e) fs -> funs fs e) (funs fs e) bs
funs fs (Let _ e f) = funs (funs fs e) f
funs fs (Typed e _) = funs fs e
funs fs _ = fs

unfold fs (Lambda v e) = Lambda v (unfold fs e)
unfold fs (Con c es) = Con c (map (unfold fs) es)
unfold fs e@(Func f) = case lookup f fs of
                       Nothing -> error ("Undefined function: "++f)
                       Just e  -> Unfold f e (unfold fs e)
unfold fs (App e f) = App (unfold fs e) (unfold fs f)
unfold fs (Case e bs) = Case (unfold fs e) (map (\(Branch c vs e) -> (Branch c vs $ unfold fs e)) bs)
unfold fs (Let v e f) = Let v (unfold fs e) (unfold fs f)
unfold fs (Typed e t) = Typed (unfold fs e) t
unfold _ e = e

shift 0 d e = e
shift i d e@(Bound j)
 | j >= d = Bound (j + i)
 | otherwise = e
shift i d (Lambda v e) = Lambda v (shift i (d + 1) e)
shift i d (Con c es) = Con c (map (shift i d) es)
shift i d (App e f) = App (shift i d e) (shift i d f)
shift i d (Case e bs) = Case (shift i d e) (map (\(Branch c vs e) -> (Branch c vs $ shift i (d + length vs) e)) bs)
shift i d (Let v e f) = Let v (shift i d e) (shift i (d + 1) f)
shift i d (Unfold v e f) = Unfold v (shift i d e) (shift i d f)
shift i d (Fold f e) = Fold f (shift i d e)
shift i d (Typed e t) = Typed (shift i d e) t
shift _ _ e = e

subst i e e'@(Bound i')
 | i' < i = e'
 | i' == i = shift i 0 e
 | otherwise = Bound (i' - 1) 
subst i e (Lambda v e') = Lambda v (subst (i + 1) e e')
subst i e (Con c es) = Con c (map (subst i e) es)
subst i e (App e' f) = App (subst i e e') (subst i e f)
subst i e (Case e' bs) = Case (subst i e e') (map (\(Branch c vs f) -> (Branch c vs $ subst (i + length vs) e f)) bs)
subst i e (Let v e' f) = Let v (subst i e e') (subst (i + 1) e f)
subst i e (Unfold v e' f) = Unfold v (subst i e e') (subst i e f)
subst i e (Fold f e') = Fold f (subst i e e')
subst i e (Typed e' t) = Typed (subst i e e') t
subst _ _ e = e

abstract i b e@(Var v) 
 | v == b = Bound i 
 | otherwise = e
abstract i b e@(Bound i') 
 | i' >= i = Bound (i' + 1)
 | otherwise = e
abstract i b (Lambda v e) = Lambda v (abstract (i + 1) b e)
abstract i b (Con c es) = Con c (map (abstract i b) es)
abstract i b (App e f) = App (abstract i b e) (abstract i b f)
abstract i b e@(Func _) = e
abstract i b (Case e bs) = Case (abstract i b e) (map (\(Branch c vs e) -> (Branch c vs $ abstract (i + length vs) b e)) bs)
abstract i b (Let v e f) = Let v (abstract i b e) (abstract (i + 1) b f)
abstract i b (Unfold v e f) = Unfold v (abstract i b e) (abstract i b f)
abstract i b (Fold v e) = Fold v (abstract i b e)
abstract i b (Typed e t) = Typed (abstract i b e) t
abstract i b e = e

rename s e@(Var v) = case lookup v s of
                       Just v'  -> Var v'
                       Nothing -> e
rename s (Bound i) = Bound i
rename s (Lambda v e) = Lambda v (rename s e)
rename s (Con c es) = Con c (map (rename s) es)
rename s (App e f) = App (rename s e) (rename s f)
rename s e@(Func _) = e
rename s (Case e bs) = Case (rename s e) (map (\(Branch c vs e) -> (Branch c vs $ rename s e)) bs)
rename s (Let v e f) = Let v (rename s e) (rename s f)
rename s (Unfold v e f) = Unfold v (rename s e) (rename s f)
rename s (Fold f e) = Fold f (rename s e)
rename s (Typed e t) = Typed (rename s e) t

replace t u v 
 | t == v = u
 | otherwise = replace' t u v

replace' e f (Lambda v e') = Lambda v (replace (shift 1 0 e) (shift 1 0 f) e')
replace' e f (Con c es) = Con c (map (replace e f) es)
replace' e f (App e' f') = App (replace e f e') (replace e f f')
replace' e f (Case e' bs) = Case (replace e f e') (map (\(Branch c vs e') -> (Branch c vs $ replace (shift (length vs) 0 e) (shift (length vs) 0 e) e')) bs)
replace' e f (Let v e' f') = Let v (replace' e f e') (replace' (shift 1 0 e) (shift 1 0 f) f')
replace' e f (Unfold v e' f') = Unfold v (replace' e f e') (replace' e f f')
replace' e f (Fold f' e') = Fold f' (replace' e f e')
replace' e f (Typed e' t) = Typed (replace e f e') t
replace' _ _ e = e

renamevar vs v
 | v `elem` vs = renamevar vs (v ++ "'")
 | otherwise = v

stripLambda (Lambda v e) = let v' = renamevar (free e) v
                               (vs, e') = stripLambda (subst 0 (Var v') e)
                           in  (v':vs, e')
stripLambda e = ([], e)

isList (Con "Nil" []) = True
isList (Con "Cons" [h,t]) = isList t
isList _ = False

list2con [] = Con "Nil" []
list2con (h:t) = Con "Cons" [h,list2con t]

con2list (Con "Nil" [])  = []
con2list (Con "Cons" [h,t]) = h:con2list t

isNat (Con "Zero" []) = True
isNat (Con "Succ" [n]) = isNat n
isNat _ = False

nat2con 0 = Con "Zero" []
nat2con n = Con "Succ" [nat2con (n-1)]

con2nat (Con "Zero" [])  = 0
con2nat (Con "Succ" [n]) = 1+con2nat n