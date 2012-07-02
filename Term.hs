module Term where

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

data Term = Var String
          | Lambda String Term
          | Con String [Term]
          | App Term Term
          | Func String
          | Case Term [Branch]
          | Let String Term Term
          | Letrec String Term Term
          | Unfold String Term
          | Label String Term
          | Subst Term Term
          | Lit L.HsLiteral
          | Typed Term L.HsQualType

data Branch = Branch String [String] Term

data Program = Program [L.HsImportDecl] [L.HsDecl] L.HsDecl Term [(String,Term)]

instance Show Term where
    show = prettyShow
    
instance Show Program where
    show = prettyShow

instance Show Branch where
    show = prettyShow

instance Eq Term where
  (==)  t t' = eqTerm t t' []

eqTerm (Var x) (Var x') fs = x==x'
eqTerm (Lambda x t) (Lambda x' t') fs = eqTerm (rename [(x,x')] t) t' fs
eqTerm (Con c ts) (Con c' ts') fs | c==c' && length ts == length ts' = all (\(t,t') -> eqTerm t t' fs) (zip ts ts')
eqTerm (App t u) (App t' u') fs = (eqTerm t t' fs) && (eqTerm u u' fs)
eqTerm (Func f) (Func f') fs = f==f'
eqTerm (Case t bs) (Case t' bs') fs | match bs bs' = (eqTerm t t' fs) && (all (\ ((Branch c xs t), (Branch c' xs' t')) -> eqTerm (rename (zip xs xs') t) t' fs) (zip bs bs'))
eqTerm (Let x t u) (Let x' t' u') fs = (eqTerm t t' fs) && (eqTerm u u' fs)
eqTerm (Unfold f t) (Unfold f' t') fs | f==f' = if f `elem` fs then True else eqTerm t t' (f:fs)
eqTerm (Subst t u) (Subst t' u') fs | eqTerm t t' fs = eqTerm u u' fs
eqTerm t (Unfold f t') fs = if f `elem` fs then False else eqTerm t t' (f:fs)
eqTerm t (Subst t' u) fs = eqTerm t u fs
eqTerm (Lit l) (Lit l') fs = l == l'
eqTerm (Typed t q) (Typed t' q') fs = eqTerm t t' fs && q == q'
eqTerm t t' fs = False
   
  
instance Pretty Program where
   pretty (Program imports dataDecls main root funcs) = vcat $ (punctuate (text "\n") $ (text ("module Main(main) where\n")):(map (text . prettyPrint) imports) ++ (map (text . prettyPrint) dataDecls) ++ [text $ prettyPrint main] ++ (map prettyFunction (("root", root):funcs)))

prettyCons (Con "Nil" []) = text "[]"
prettyCons (Con "Cons" ((Con "Nil" []):[])) = text "[]"
prettyCons (Con "Cons" (x:Con "Nil" []:[])) = brackets $ pretty x
prettyCons (Con "Cons" (x:[])) = parens (pretty x)
prettyCons (Con "Cons" (x:xs)) = parens (pretty x <> colon <> pretty (Con "Cons" xs))

instance Pretty Term where
   pretty (Var v)
         | v `elem` [".", ":", "$", "$$", "==", "<=", ">=", "<", ">", "=<<", "+", "-", "/", "*", "!!", "/=", "%", "++"] = parens $ text v
    	 | otherwise = text v
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
                            in  parens $ text "\\" <> hsep (map (parens . text) vs) <+> text "->" <+> pretty f
   pretty (Typed e t) = parens (parens (pretty e) <+> text "::" <> text (prettyPrint t))
   

instance Pretty Branch where
   pretty (Branch c [] e) 
    | c == "Nil" = text "[]" <+> text "->" <+> pretty e
    | otherwise = text c <+> text "->" <+> pretty e
   pretty (Branch c vs e) = (if c == "Nil"
                                then (text "[]")
                                else if c == "Cons"
                                      then pretty (Con c (map Var vs))
                                      else text c <+> (hcat (punctuate space (map (parens . text) vs)))) <+> text "->" <+> pretty e $$ empty
                            
prettyFunction :: (String, Term) -> Doc
prettyFunction (name, body) = text name <+> text "=" <+> pretty body

renaming (Var a) (Var a') fs s = if a `elem` fst (unzip s)
                                   then if (a,a') `elem` s then Just s else Nothing
                                   else Just ((a,a'):s)
renaming (Lambda x t) (Lambda x' t') fs s = renaming t t' fs ((x,x'):s)
renaming (Con c ts) (Con c' ts') fs s | c==c' && length ts == length ts' = foldrM (\(t,t') s -> renaming t t' fs s) s (zip ts ts')
renaming (App t u) (App t' u') fs s = (renaming t t' fs s) >>= (renaming u u' fs)
renaming (Func f) (Func f') fs s | f==f' = Just s
renaming (Case t bs) (Case t' bs') fs s | match bs bs' = (renaming t t' fs s) >>= (\s -> foldrM (\((Branch c xs t),(Branch c' xs' t')) s -> renaming t t' fs ((zip xs xs')++s)) s (zip bs bs'))
renaming (Let x t u) (Let x' t' u') fs s = (renaming t t' fs s) >>= (\s -> renaming u u' fs ((x,x'):s))
renaming (Letrec f t u) (Letrec f' t' u') fs s = (renaming t t' fs ((f,f'):s)) >>= (renaming u u' fs)
renaming (Unfold f t) (Unfold f' t') fs s = if f `elem` fst (unzip s)
                                            then if (f,f') `elem` s then Just s else Nothing
                                            else renaming t t' (f':fs) ((f,f'):s)
renaming (Subst t u) (Subst t' u') fs s | isJust (renaming t t' [] []) = renaming u u' fs s
renaming t (Unfold f t') fs s = if f `elem` fs then Nothing else renaming t t' (f:fs) s
renaming t (Subst t' u) fs s = renaming t u fs s
renaming (Typed e t) (Typed e' t') fs s | t == t' = renaming e e' fs s
renaming t t' fs s = Nothing

embedding t u fs s = mplus (couple t u fs s) (dive t u fs s)

couple (Var x) (Var x') fs s = if x `elem` fst (unzip s)
                                 then if (x,x') `elem` s then Just s else Nothing
                                 else Just ((x,x'):s)
couple (Lambda x t) (Lambda x' t') fs s = embedding t t' fs ((x,x'):s)
couple (Con c ts) (Con c' ts') fs s | c == c' && length ts == length ts' = foldrM (\ (t,t') s -> embedding t t' fs s) s (zip ts ts')
couple (App t u) (App t' u') fs s = (couple t t' fs s) >>= (embedding u u' fs)
couple (Func f) (Func f') fs s | f == f' = Just s
couple (Case t bs) (Case t' bs') fs s | match bs bs' = (embedding t t' fs s) >>= (\s->foldrM (\ ((Branch c xs t),(Branch c' xs' t')) s -> embedding t t' fs ((zip xs xs')++s)) s (zip bs bs'))
couple (Let x t u) (Let x' t' u') fs s = (embedding t t' fs s) >>= (\s -> embedding u u' fs ((x,x'):s))
couple (Unfold f t) (Unfold f' t') fs s = if f `elem` fs
                                          then if (f,f') `elem` s then Just s else Nothing
                                          else embedding t t' (f':fs) ((f,f'):s)
couple (Subst t u) (Subst t' u') fs s | isJust (renaming t t' [] []) = embedding u u' fs s
couple (Typed e t) (Typed e' t') fs s | t == t' = embedding e e' fs s
couple (Lit l) (Lit l') fs s | l == l' = Just s
couple t u fs s = Nothing

dive t (Lambda x t') fs s = embedding t t' fs s
dive t (Con c ts) fs s = msum (map (\t' -> embedding t t' fs s) ts)
dive t (App t' u) fs s = mplus (embedding t t' fs s) (embedding t u fs s)
dive t (Case t' bs) fs s = mplus (embedding t t' fs s) (msum (map (\(Branch c xs t') -> embedding t t' fs s) bs))
dive t (Let x t' u) fs s = mplus (embedding t t' fs s) (embedding t u fs s)
dive t (Unfold f t') fs s = if f `elem` fs then Nothing else embedding t t' (f:fs) s
dive t (Subst t' u) fs s = embedding t u fs s
dive t (Typed e' _) fs s = embedding t e' fs s
dive t u fs s = Nothing

generalise (Var x) (Var x') fs s fv bv = (Var x,s)
generalise (Lambda x t) (Lambda x' t') fs s fv bv = let (t'',s') = generalise t t' fs s fv (x:bv)
                                                    in (Lambda x t'',s')
generalise (Con c ts) (Con c' ts') fs s fv bv | c==c' && length ts == length ts' = let (ts'',s') = foldr (\(t,t') (ts,s) -> let (t'',s') = generalise t t' fs s fv bv
                                                                                                                            in (t'':ts,s')) ([],s) (zip ts ts')
                                                                                   in (Con c ts'',s')
generalise (App t u) (App t' u') fs s fv bv | isJust (couple t' t [] []) = let (t'',s') = generalise t t' fs s fv bv
                                                                               (u'',s'') = generalise u u' fs s' fv bv
                                                                           in (App t'' u'',s'')
generalise (Func f) (Func f') fs s fv bv | f==f' = (Func f,s)
generalise (Case t bs) (Case t' bs') fs s fv bv | match bs bs' = let (t'',s') = generalise t t' fs s fv bv
                                                                     (bs'',s'') = foldr (\ ((Branch c xs t),(Branch c' xs' u)) (bs,s) -> let (t'',s') = generalise t u fs s fv (xs++bv)
                                                                                                                                         in ((Branch c xs t''):bs,s')) ([],s') (zip bs bs')
                                                                 in (Case t'' bs'',s'')
generalise (Let x t u) (Let x' t' u') fs s fv bv = if t'==t
                                                   then generalise (rename [(x,x')] u) u' fs s fv bv
                                                   else let (t'',s') = generalise t t' fs s fv bv
                                                            (u'',s'') = generalise u u' fs s' fv (x:bv)
                                                        in (Let x t'' u'',s'')
generalise (Unfold f t) (Unfold f' t') fs s fv bv = if f' `elem` fs
                                                    then (Unfold f t,s)
                                                    else let (t'',s') = generalise t t' (f':fs) s fv bv
                                                         in (Unfold f t'',s')
generalise (Label l t) (Label l' t') fs s fv bv = let (t'',s') = generalise t t' fs s fv bv
                                                  in  (Label l t'',s')
generalise (Subst t u) (Subst t' u') fs s fv bv | isJust (renaming t t' [] []) = let (u'',s') = generalise u u' fs s fv bv
                                                                                 in (Subst t u'',s')
generalise (Unfold f t) t' fs s fv bv = generalise t t' fs s fv bv
generalise (Subst t u) t' fs s fv bv = generalise u t' fs s fv bv
generalise (Typed e t) (Typed e' t') fs s fv bv | t == t' = let (e'', s') = generalise e e' fs s fv bv
                                                              in (Typed e'' t, s')
generalise t t' fs s fv bv = let xs = intersect (free t) bv
                                 u = foldr (\x t -> Lambda x t) t xs
                                 (vs,_) = unzip s
                                 x = renamevar (vs++bv++fv) "x"
                             in (foldr (\t u -> App u t) (Var x) (map Var xs),(x,u):s)

makeLet s t = foldr (\(x,t) u -> Let x t u) t s

residualise (Var x) r d = (Var x,d)
residualise (Lambda x t) r d = let (t',d') = residualise t r d
                               in  (Lambda x t',d')
residualise (Con c ts) r d = let (ts',d') = foldr (\t (ts,d) -> let (t',d') = residualise t r d
                                                                in (t':ts,d')) ([],d) ts
                             in (Con c ts',d')
residualise (App t u) r d = let (t',d') = residualise t r d
                                (u',d'') = residualise u r d'
                            in  (App t' u',d'')
residualise (Func f) r d = error ("Function not unfolded: " ++ f)
residualise (Case t bs) r d = let (t',d') = residualise t r d
                                  (bs',d'') = foldr (\(Branch c xs t) (bs,d) -> let (t',d') = residualise t r d
                                                                         in  ((Branch c xs t'):bs,d')) ([],d') bs
                              in  (Case t' bs',d'')
residualise (Let x t u) r d = let (t',d') = residualise t r d
                                  (u',d'') = residualise u r d'
                              in  (subst x t' u',d'')
residualise (Unfold f t) r d = residualise t r d
residualise (Label l t) r d = case (find (\(l',f) -> l==l') r) of
                                 Just (l',f) -> let xs = free t
                                                in (foldr (\x t -> App t (Var x)) (Func f) xs,d)
                                 Nothing -> if l `elem` labs t
                                            then let (fs,_) = unzip d
                                                     (_,fs') = unzip r
                                                     f = renamevar (fs++fs') "f"
                                                     xs = free t
                                                     (t',d') = residualise t ((l,f):r) d
                                                in  (foldr (\x t -> App t (Var x)) (Func f) xs,(f,foldl (\t x -> Lambda x t) t' xs):d')
                                            else residualise t r d
residualise (Subst t u) r d = residualise u r d
residualise (Typed f t) r d = let (f', d') = residualise f r d
                              in (Typed f' t, d')
residualise e _ d = (e, d)

addLetrecs fn fs (Var x) = Var x
addLetrecs fn fs (Lambda x t) = Lambda x (addLetrecs fn fs t)
addLetrecs fn fs (Con c ts) = Con c (map (addLetrecs fn fs) ts)
addLetrecs fn fs (App t u) = App (addLetrecs fn fs t) (addLetrecs fn fs u)
addLetrecs fn fs (Func f) = case (lookup f fs) of
                              Nothing -> error ("Undefined function: "++f)
                              Just t  -> if   f `elem` fn
                                         then Func f
                                         else Letrec f (addLetrecs (f:fn) fs t) (Func f)
addLetrecs fn fs (Case t bs) = Case (addLetrecs fn fs t) (map (\(Branch c xs t) -> (Branch c xs $ addLetrecs fn fs t)) bs)
addLetrecs fn fs (Let x t u) = Let x (addLetrecs fn fs t) (addLetrecs fn fs u)
addLetrecs fn fs (Typed e t) = Typed (addLetrecs fn fs e) t
addLetrecs fn fs (Lit l) = Lit l

match bs bs' = (length bs == length bs') && (all (\((Branch c xs t),(Branch c' xs' t')) -> c == c' && length xs == length xs') (zip bs bs'))

free t = free' [] [] [] t

free' fv bv fs (Var x) = if (x `elem` (fv++bv)) then fv else x:fv
free' fv bv fs (Lambda x t) = free' fv (x:bv) fs t
free' fv bv fs (Con c ts) = foldr (\t fv -> free' fv bv fs t) fv ts
free' fv bv fs (App t u) = free' (free' fv bv fs t) bv fs u
free' fv bv fs (Func f) = fv
free' fv bv fs (Case t bs) = foldr (\(Branch c xs t) fv -> free' fv (xs++bv) fs t) (free' fv bv fs t) bs
free' fv bv fs (Let x t u) = free' (free' fv (x:bv) fs u) bv fs t
free' fv bv fs (Unfold f t) = if f `elem` fs then fv else free' fv bv (f:fs) t
free' fv bv fs (Label l t) = free' fv bv fs t
free' fv bv fs (Subst t u) = free' fv bv fs u
free' fv bv fs (Typed t _) = free' fv bv fs t
free' fv _ _ _ = fv

labs t = labs' [] [] t

labs' ls fs (Var x) = ls
labs' ls fs (Lambda x t) = labs' ls fs t
labs' ls fs (Con c ts) = foldr (\t ls -> labs' ls fs t) ls ts
labs' ls fs (Func f) = ls
labs' ls fs (App t u) = labs' (labs' ls fs t) fs u
labs' ls fs (Case t bs) = foldr (\(Branch c xs t) ls -> labs' ls fs t) (labs' ls fs t) bs
labs' ls fs (Let x t u) = labs' (labs' ls fs t) fs u
labs' ls fs (Unfold f t) = if f `elem` fs then ls else labs' ls (f:fs) t
labs' ls fs (Label l t) = labs' (l:ls) fs t
labs' ls fs (Subst t u) = labs' ls fs u
labs' ls fs (Typed e _) = labs' ls fs e
labs' ls _ _ = ls

rename [] t = t
rename s (Var x) = case (lookup x s) of
                       Just x'  -> Var x'
                       Nothing -> Var x
rename s (Lambda x t) = let x' = renamevar (snd (unzip s)) x
                        in Lambda x' (rename ((x,x'):s) t)
rename s (Con c ts) = Con c (map (rename s) ts)
rename s (App t u) = App (rename s t) (rename s u)
rename s (Func f) = Func f
rename s (Case t bs) = Case (rename s t) (map (\(Branch c xs t) -> let fv = foldr (\x fv -> let x' = renamevar fv x in x':fv) (snd (unzip s)) xs
                                                                       xs' = take (length xs) fv
                                                                   in (Branch c xs' $ rename ((zip xs xs')++s) t)) bs)
rename s (Let x t u) = let x' = renamevar (snd (unzip s)) x
                       in Let x' (rename s t) (rename ((x,x'):s) u)
rename s (Letrec f t u) = Letrec f (rename s t) (rename s u)
rename s (Unfold f t) = Unfold f (rename s t)
rename s (Subst t u) = Subst (rename s t) (rename s u)
rename s (Typed e t) = Typed (rename s e) t
rename _ (Lit l) = Lit l

subst x t (Var x') = if x==x' then t else Var x'
subst x t (Lambda x' t') = if   x==x'
                           then Lambda x' t'
                           else let x'' = renamevar (free t) x'
                                in Lambda x'' (subst x t (rename [(x',x'')] t'))
subst x t (Con c ts) = Con c (map (subst x t) ts)
subst x t (App t' u) = App (subst x t t') (subst x t u)
subst x t (Func f) = Func f
subst x t (Case t' bs) = Case (subst x t t') (map (\(Branch c xs t') -> if x `elem` xs
                                                                 then (Branch c xs t')
                                                                 else let fv = foldr (\x fv -> let x' = renamevar fv x in x':fv) (free t) xs
                                                                          xs' = take (length xs) fv
                                                                      in (Branch c xs' $ subst x t (rename (zip xs xs') t'))) bs)
subst x t (Let x' t' u) = if   x==x'
                          then Let x' (subst x t t') u
                          else let x'' = renamevar (free t) x'
                               in Let x'' (subst x t t') (subst x t (rename [(x',x'')] u))
subst x t (Letrec f t' u) = Letrec f (subst x t t') (subst x t u)
subst x t (Unfold f t') = Unfold f (subst x t t')
subst x t (Subst t' u) = Subst t' (subst x t u)
subst x t (Typed e q) = Typed (subst x t e) q
subst _ _ (Lit l) = Lit l

renameFun f f' (Var x) = Var x
renameFun f f' (Lambda x t) = Lambda x (renameFun f f' t)
renameFun f f' (Con c ts) = Con c (map (renameFun f f') ts)
renameFun f f' (App t u) = App (renameFun f f' t) (renameFun f f' u)
renameFun f f' (Func g) = if f==g then Func f' else Func g
renameFun f f' (Case t bs) = Case (renameFun f f' t) (map (\(Branch c xs t) -> (Branch c xs $ renameFun f f' t)) bs)
renameFun f f' (Let x t u) = Let x (renameFun f f' t) (renameFun f f' u)
renameFun f f' (Letrec g t u) = Letrec g (renameFun f f' t) (renameFun f f' u)
renameFun f f' (Unfold g t) = Unfold g (renameFun f f' t)
renameFun f f' (Subst t u) = Subst (renameFun f f' t) (renameFun f f' u)
renameFun f f' (Typed e t) = Typed (renameFun f f' e) t
renameFun _ _ (Lit l) = Lit l

renamevar xs x = if   x `elem` xs
                 then renamevar xs (x++"'")
                 else x

stripLambda (Lambda x t) = let (xs,t') = stripLambda t
                           in  (x:xs,t')
stripLambda t = ([],t)

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