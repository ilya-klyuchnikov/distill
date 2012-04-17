{- Term Language Definition, Parser and Pretty Printer -}

module Core.Term where

import Data.Char
import Data.Maybe
import Data.List (intersect)
import Data.Foldable (foldrM,find)
import Control.Monad
import Text.PrettyPrint.HughesPJ
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Debug.Trace
import qualified Language.Haskell.Syntax as L

data Term = Free String
          | Bound Int
          | Lambda String Term
          | Con String [Term]
          | Apply Term Term
          | Fun String
          | Case Term [(String,[String],Term)]
          | Let String Term Term
          | Unfold String Term Term
          | Fold String Term
          | Lit L.HsLiteral
          | Typed Term L.HsQualType

-- instance Show Term where
--   show t = render (prettyTerm t)

data Program = Program Term [(String,Term)]

-- instance Show Program where
--    show (Program t e) = render (prettyProg t e)

instance Eq Term where
   (==) (Free x) (Free x') = x==x'
   (==) (Bound i) (Bound i') = i==i'
   (==) (Lambda _ t) (Lambda _ t') = t==t'
   (==) (Con c ts) (Con c' ts') = c==c' && ts==ts'
   (==) (Apply t u) (Apply t' u') = (t==t') && (u==u')
   (==) (Fun f) (Fun f') = f==f'
   (==) (Case t bs) (Case t' bs') = (t == t') && (match bs bs') && (all (\ ((_,_,t),(_,_,t')) -> t == t') (zip bs bs'))
   (==) (Let _ t u) (Let _ t' u') = (t==t') && (u==u')
   (==) (Unfold f t u) (Unfold f' t' u') = (f==f') && (u==u')
   (==) (Fold f t) (Fold f' t') = (f==f')
   (==) t t' = False

renaming (Free x) (Free x') s = if x `elem` fst (unzip s)
                                then if (x,x') `elem` s then Just s else Nothing
                                else Just ((x,x'):s)
renaming (Bound i) (Bound i') s | i == i' = Just s
renaming (Lambda _ t) (Lambda _ t') s = renaming t t' s
renaming (Con c ts) (Con c' ts') s | c == c' && length ts == length ts' = foldrM (\(t,t') s -> renaming t t' s) s (zip ts ts')
renaming (Apply t u) (Apply t' u') s = (renaming t t' s) >>= (renaming u u')
renaming (Fun f) (Fun f') s | f==f' = Just s
renaming (Case t bs) (Case t' bs') s | match bs bs' = (renaming t t' s) >>= (\s -> foldrM (\((_,_,t),(_,_,t')) s -> renaming t t' s) s (zip bs bs'))
renaming (Let _ t u) (Let _ t' u') s = (renaming t t' s) >>= (renaming u u')
renaming (Unfold f t u) (Unfold f' t' u') s | f==f' = renaming u u' s
renaming (Fold f t) (Fold f' t') s | f==f' = Just s
renaming t t' s = Nothing

inst (Free x) t s = if x `elem` fst (unzip s)
                    then if (x,t) `elem` s then Just s else Nothing
                    else Just ((x,t):s)
inst (Bound i) (Bound i') s | i == i' = Just s
inst (Lambda _ t) (Lambda _ t') s = inst t t' s
inst (Con c ts) (Con c' ts') s | c == c' && length ts == length ts' = foldrM (\ (t,t') s -> inst t t' s) s (zip ts ts')
inst (Apply t u) (Apply t' u') s = (inst t t' s) >>= (inst u u')
inst (Fun f) (Fun f') s | f == f' = Just s
inst (Case t bs) (Case t' bs') s | match bs bs' = (inst t t' s) >>= (\s->foldrM (\ ((_,_,t),(_,_,t')) s -> inst t t' s) s (zip bs bs'))
inst (Let _ t u) (Let _ t' u') s = (inst t t' s) >>= (inst u u')
inst (Unfold f t u) (Unfold f' t' u') s | f==f' = inst u u' s
inst (Fold f t) (Fold f' t') s | f==f' = Just s
inst t t' s = Nothing

isRenaming [] = True
isRenaming ((x,Free x'):s) = isRenaming s
isRenaming s = False

embedding t u s = mplus (couple t u s) (dive t u s)

couple (Free x) (Free x') s = if x `elem` fst (unzip s)
                              then if (x,x') `elem` s then Just s else Nothing
                              else Just ((x,x'):s)
couple (Bound i) (Bound i') s | i == i' = Just s
couple (Lambda _ t) (Lambda _' t') s = embedding t t' s
couple (Con c' ts) (Con c'' ts') s | c' == c'' && length ts == length ts' = foldrM (\ (t,t') s -> embedding t t' s) s (zip ts ts')
couple (Apply t u) (Apply t' u') s = (couple t t' s) >>= (embedding u u')
couple (Fun f) (Fun f') s | f==f' = Just s
couple (Case t bs) (Case t' bs') s | match bs bs' = (embedding t t' s) >>= (\s->foldrM (\ ((_,_,t),(_,_,t')) s -> embedding t t' s) s (zip bs bs'))
couple (Let _ t u) (Let _ t' u') s = (embedding t t' s) >>= (embedding u u')
couple (Unfold f t u) (Unfold f' t' u') s | f==f' = embedding u u' s
couple (Fold f t) (Fold f' t') s | f==f' = Just s
couple t u s = Nothing

dive t (Con _ ts) s = msum (map (\t' -> embedding t t' s) ts)
dive t (Apply t' u) s = mplus (embedding t t' s) (embedding t u s)
dive t (Case t' bs) s = mplus (embedding t t' s) (msum (map (\(_,vs,t') -> embedding t (shift (length vs) 0 t') s) bs))
dive t (Unfold f _ t') s = embedding t t' s
dive t u s = Nothing

generalise (Free x) (Free x') s fv bv = (Free x,s)
generalise (Bound i) (Bound i') s fv bv | i==i' = (Bound i,s)
generalise (Lambda x t) (Lambda x' t') s fv bv = let (gv,_) = unzip s
                                                     x'' = renamevar (gv++fv) x
                                                     (t'',s') = generalise (subst 0 (Free x'') t) (subst 0 (Free x'') t') s (x'':fv) (x'':bv)
                                                 in (Lambda x (abstract 0 x'' t''),s')
generalise (Con c ts) (Con c' ts') s fv bv | c==c' && length ts == length ts' = let (ts'',s') = foldr (\(t,t') (ts,s) -> let (t'',s') = generalise t t' s fv bv
                                                                                                                         in (t'':ts,s')) ([],s) (zip ts ts')
                                                                                in (Con c ts'',s')
generalise (Apply t u) (Apply t' u') s fv bv | isJust (couple t t' []) = let (t'',s') = generalise t t' s fv bv
                                                                             (u'',s'') = generalise u u' s' fv bv
                                                                         in (Apply t'' u'',s'')
generalise (Fun f) (Fun f') s fv bv | f==f' = (Fun f,s)
generalise (Case t bs) (Case t' bs') s fv bv | match bs bs' = let (t'',s') = generalise t t' s fv bv
                                                                  (bs'',s'') = foldr (\ ((c,xs,t),(c',xs',u)) (bs,s) -> let (gv,_) = unzip s
                                                                                                                            fv' = foldr (\x fv -> let x'=renamevar (gv++fv) x in x':fv) fv xs
                                                                                                                            xs'' = take (length xs) fv'
                                                                                                                            t' = foldr (\x t -> subst 0 (Free x) t) t xs''
                                                                                                                            u' = foldr (\x t -> subst 0 (Free x) t) u xs''
                                                                                                                            (t'',s') = generalise t' u' s fv' (xs''++bv)
                                                                                                                        in ((c',xs',foldl (\t x -> abstract 0 x t) t'' xs''):bs,s')) ([],s') (zip bs bs')
                                                              in (Case t'' bs'',s'')
generalise (Let x t u) (Let x' t' u') s fv bv = let x'' = renamevar (fst (unzip s)++fv) x
                                                    (t'',s') = generalise t t' s fv bv
                                                    (u'',s'') = generalise (subst 0 (Free x'') u) (subst 0 (Free x'') u') s' (x'':fv) (x'':bv)
                                                in (Let x t'' (abstract 0 x'' u''),s'')
generalise (Unfold f t u) (Unfold f' t' u') s fv bv | f==f' = let (u'',s') = generalise u u' s fv bv
                                                              in (Unfold f t u'',s')
generalise (Fold f t) (Fold f' t') s fv bv | f==f' = (Fold f t,s)
generalise t t' s fv bv = let xs = intersect (free t) bv
                              t'' = foldr (\x t -> Lambda x (abstract 0 x t)) t xs
                              (gv,_) = unzip s
                              x = renamevar (gv++fv) "x"
                          in (foldr (\t e -> Apply e t) (Free x) (map Free xs),(x,t''):s)

extract s s' t = foldr (\(x,t) u -> case t of
                                       Free x' -> subst 0 (Free x') (abstract 0 x u)
                                       _       -> case (find (\ (x',t') -> t==t') s') of
                                                     Just (x',t') -> subst 0 (Free x') (abstract 0 x u)
                                                     Nothing -> Let x t (abstract 0 x u)) t s

residualise (Free x) fv r d = (Free x,d)
residualise (Bound i) fv r d = (Bound i,d)
residualise (Lambda x t) fv r d = let x' = renamevar fv x
                                      (t',d') = residualise (subst 0 (Free x') t) (x':fv) r d
                                  in  (Lambda x (abstract 0 x' t'),d')
residualise (Con c ts) fv r d = let (ts',d') = foldr (\t (ts,d) -> let (t',d') = residualise t fv r d
                                                                   in (t':ts,d')) ([],d) ts
                                in (Con c ts',d')
residualise (Apply t u) fv r d = let (t',d') = residualise t fv r d
                                     (u',d'') = residualise u fv r d'
                                 in  (Apply t' u',d'')
residualise (Fun f) fv r d = error ("Function not unfolded: " ++ f)
residualise (Case t bs) fv r d = let (t',d') = residualise t fv r d
                                     (bs',d'') = foldr (\(c,xs,t) (bs,d) -> let fv' = foldr (\x fv -> let x'=renamevar fv x in x':fv) fv xs
                                                                                xs' = take (length xs) fv'
                                                                                (t',d') = residualise (foldr (\x t -> subst 0 (Free x) t) t xs') fv' r d
                                                                            in  ((c,xs,foldl (\t x -> abstract 0 x t) t' xs'):bs,d')) ([],d') bs
                                 in  (Case t' bs',d'')
residualise (Let x t u) fv r d = let x' = renamevar fv x
                                     (t',d') = residualise t fv r d
                                     (u',d'') = residualise (subst 0 (Free x') u) (x':fv) r d'
                                 in  (subst 0 t' (abstract 0 x' u'),d'')
residualise (Unfold f t u) fv r d = let (fs,_) = unzip d
                                        (fs',_) = unzip r
                                        f' = renamevar (fs++fs') "f"
                                        (u',d') = residualise u fv ((f',t):r) d
                                    in  if f' `elem` (funs [] u')
                                        then let xs = free t
                                             in  (foldr (\x t -> Apply t (Free x)) (Fun f') xs,(f',foldl (\t x -> Lambda x (abstract 0 x t)) u' xs):d')
                                        else (u',d')
residualise (Fold f t) fv r d = case (find (\(f',t') -> isJust (renaming t' t [])) r) of
                                   Just (f',t') -> let xs = free t
                                                   in (foldr (\x t -> Apply t (Free x)) (Fun f') xs,d)
                                   Nothing -> error ("Fold has no matching unfold: " ++ f)

match bs bs' = (length bs == length bs') && (all (\((c,xs,t),(c',xs',t')) -> c == c' && length xs == length xs') (zip bs bs'))

free t = free' [] t

free' xs (Free x) = if (x `elem` xs) then xs else x:xs
free' xs (Bound _) = xs
free' xs (Lambda _ t) = free' xs t
free' xs (Con _ ts) = foldr (\t xs -> free' xs t) xs ts
free' xs (Apply t u) = free' (free' xs t) u
free' xs (Fun _) = xs
free' xs (Case t bs) = foldr (\(_,_,t) xs' -> free' xs' t) (free' xs t) bs
free' xs (Let _ t u) = free' (free' xs t) u
free' xs (Unfold f t u) = free' xs u
free' xs (Fold f t) = xs

bound t = bound' 0 [] t

bound' d bs (Free _) = bs
bound' d bs (Bound i) = if   (i>=d)
                        then if   (i-d) `elem` bs
                             then bs
                             else (i-d):bs
                        else bs
bound' d bs (Lambda _ t) = bound' (d+1) bs t
bound' d bs (Con _ ts) = foldr (\t bs -> bound' d bs t) bs ts
bound' d bs (Apply t u) = bound' d (bound' d bs u) t
bound' d bs (Fun _) = bs
bound' d bs (Case t bs') = foldr (\(_,xs,t) bs -> bound' (d+length xs) bs t) (bound' d bs t) bs'
bound' d bs (Let _ t u) = bound' d (bound' (d+1) bs u) t
bound' d bs (Unfold f t u) = bound' d bs u
bound' d bs (Fold f t) = bs

funs fs (Free x) = fs
funs fs (Bound i) = fs
funs fs (Lambda x t) = funs fs t
funs fs (Con c ts) = foldr (\t fs -> funs fs t) fs ts
funs fs (Fun f) = f:fs
funs fs (Apply t u) = funs (funs fs t)  u
funs fs (Case t bs) = foldr (\(_,_,t) fs -> funs fs t) (funs fs t) bs
funs fs (Let x t u) = funs (funs fs t) u
funs fs (Unfold f t u) = fs
funs fs (Fold f t) = fs

unfold fs (Free x) = Free x
unfold fs (Bound i) = Bound i
unfold fs (Lambda x t) = Lambda x (unfold fs t)
unfold fs (Con c ts) = Con c (map (unfold fs) ts)
unfold fs (Fun f) = case (lookup f fs) of
                       Nothing -> error ("Undefined function: "++f)
                       Just t  -> Unfold f (Fun f) (unfold fs t)
unfold fs (Apply t u) = Apply (unfold fs t) (unfold fs u)
unfold fs (Case t bs) = Case (unfold fs t) (map (\(c,xs,t) -> (c,xs,unfold fs t)) bs)
unfold fs (Let x t u) = Let x (unfold fs t) (unfold fs u)
unfold fs (Unfold f t u) = Unfold f t u
unfold fs (Fold f t) = Fold f t


shift 0 d u = u
shift i d (Free x) = Free x
shift i d (Bound j) = if j >= d then Bound (j+i) else Bound j
shift i d (Lambda x t) = Lambda x (shift i (d+1) t)
shift i d (Con c ts) = Con c (map (shift i d) ts)
shift i d (Apply t u) = Apply (shift i d t) (shift i d u)
shift i d (Fun f) = Fun f
shift i d (Case t bs) = Case (shift i d t) (map (\(c,xs,t) -> (c,xs,shift i (d+length xs) t)) bs)
shift i d (Let x t u) = Let x (shift i d t) (shift i (d+1) u)
shift i d (Unfold f t u) = Unfold f (shift i d t) (shift i d u)
shift i d (Fold f t) = Fold f (shift i d t)

subst i t (Free x) = Free x
subst i t (Bound i') = if   i'<i
                       then Bound i'
                       else if   i'==i
                            then shift i 0 t
                            else Bound (i'-1)
subst i t (Lambda x t') = Lambda x (subst (i+1) t t')
subst i t (Con c ts) = Con c (map (subst i t) ts)
subst i t (Apply t' u) = Apply (subst i t t') (subst i t u)
subst i t (Fun f) = Fun f
subst i t (Case t' bs) = Case (subst i t t') (map (\(c,xs,u) -> (c,xs,subst (i+length xs) t u)) bs)
subst i t (Let x t' u) = Let x (subst i t t') (subst (i+1) t u)
subst i t (Unfold f t' u) = Unfold f (subst i t t') (subst i t u)
subst i t (Fold f t') = Fold f (subst i t t')

abstract i b (Free x) = if x==b then Bound i else Free x
abstract i b (Bound i') = if i'>=i then Bound (i'+1) else Bound i'
abstract i b (Lambda x t) = Lambda x (abstract (i+1) b t)
abstract i b (Con c ts) = Con c (map (abstract i b) ts)
abstract i b (Apply t u) = Apply (abstract i b t) (abstract i b u)
abstract i b (Fun f) = Fun f
abstract i b (Case t bs) = Case (abstract i b t) (map (\(c,xs,t) -> (c,xs,abstract (i + length xs) b t)) bs)
abstract i b (Let x t u) = Let x (abstract i b t) (abstract (i+1) b u)
abstract i b (Unfold f t u) = Unfold f (abstract i b t) (abstract i b u)
abstract i b (Fold f t) = Fold f (abstract i b t)

rename s (Free x) = case (lookup x s) of
                       Just x'  -> Free x'
                       Nothing -> Free x
rename s (Bound i) = Bound i
rename s (Lambda a t) = Lambda a (rename s t)
rename s (Con c ts) = Con c (map (rename s) ts)
rename s (Apply t u) = Apply (rename s t) (rename s u)
rename s (Fun f) = Fun f
rename s (Case t bs) = Case (rename s t) (map (\(c,vs,t) -> (c,vs,rename s t)) bs)
rename s (Let x t u) = Let x (rename s t) (rename s u)
rename s (Unfold f t u) = Unfold f (rename s t) (rename s u)
rename s (Fold f t) = Fold f (rename s t)

replace t u v = if t==v then u else replace' t u v
replace' t u (Free x) = Free x
replace' t u (Bound i) = Bound i
replace' t u (Lambda x t') = Lambda x (replace (shift 1 0 t) (shift 1 0 u) t')
replace' t u (Con c ts) = Con c (map (replace t u) ts)
replace' t u (Apply t' u') = Apply (replace t u t') (replace t u u')
replace' t u (Fun f) = Fun f
replace' t u (Case t' bs) = Case (replace t u t') (map (\(c,xs,t') -> (c,xs,replace (shift (length xs) 0 t) (shift (length xs) 0 u) t')) bs)
replace' t u (Let x t' u') = Let x (replace' t u t') (replace' (shift 1 0 t) (shift 1 0 u) u')
replace' t u (Unfold f t' u') = Unfold f (replace' t u t') (replace' t u u')
replace' t u (Fold f t') = Fold f (replace' t u t')

renamevar xs x = if   x `elem` xs
                 then renamevar xs (x++"'")
                 else x

stripLambda (Lambda x t) = let x' = renamevar (free t) x
                               (xs,u) = stripLambda (subst 0 (Free x') t)
                           in  (x':xs,u)
stripLambda t = ([],t)

blank = Text.PrettyPrint.HughesPJ.space

prettyTerm (Free x) = text x
prettyTerm (Bound i) = (text "#") <> (int i)
prettyTerm t@(Lambda _ _) = let (xs,u) = stripLambda t
                            in  (text "\\") <> (hsep (map text xs)) <> (text ".") <> (prettyTerm u)
prettyTerm t@(Con c ts) = if   isNat t
                          then int (con2nat t)
                          else if isList t
                               then text "[" <> (hcat (punctuate comma (map prettyTerm (con2list t)))) <> (text "]")
                               else if ts==[]
                                    then text c
                                    else (text c) <> (parens (hcat (punctuate comma (map prettyTerm ts))))
prettyTerm t@(Apply _ _) = prettyApp t
prettyTerm (Fun f) = text f
prettyTerm (Case t (b:bs)) = hang ((text "case") <+> (prettyAtom t) <+> (text "of")) 1 (blank <+> (prettyBranch b) $$ (vcat (map (\b->(text "|" <+>) (prettyBranch b)) bs))) where
   prettyBranch (c,[],t) = (text c) <+> (text "->") <+> (prettyTerm t)
   prettyBranch (c,xs,t) = let xs' = map (renamevar (free t)) xs
                               t' = foldr (\x t->subst 0 (Free x) t) t xs'
                           in  (text c) <> (parens (hcat (punctuate comma (map text xs')))) <+> (text "->") <+> (prettyTerm t') $$ empty
prettyTerm (Let x t u) = let x' = renamevar (free u) x
                         in  ((text "let") <+> (text x') <+> (text "=") <+> (prettyTerm t)) $$ ((text "in") <+> (prettyTerm (subst 0 (Free x') u)))
prettyTerm (Unfold f t u) = (text "Unfold") <+> (prettyAtom t) <+> (prettyAtom u)
prettyTerm (Fold f t) = (text "Fold") <+> (prettyAtom t)

prettyApp (Apply t u) = (prettyApp t) <+> (prettyAtom u)
prettyApp t = prettyAtom t

prettyAtom (Free x) = text x
prettyAtom (Bound i) = (text "#") <> (int i)
prettyAtom t@(Con c ts) = if   isNat t
                          then int (con2nat t)
                          else if isList t
                               then text "[" <> (hcat (punctuate comma (map prettyTerm (con2list t)))) <> (text "]")
                               else if ts==[]
                                    then text c
                                    else (text c) <> (parens (hcat (punctuate comma (map prettyTerm ts))))
prettyAtom (Fun f) = text f
prettyAtom t = parens (prettyTerm t)

prettyEnv [] = empty
prettyEnv ((f,t):e) = (text f) <+> equals <+> (prettyTerm t) <> semi $$ (prettyEnv e)

prettyProg t e = (prettyTerm t) $$ (text "where") $$ (prettyEnv e)
{-
drawLTS t = render ((text "digraph G {\norientation=landscape\nratio=compress;\nsize=\"10,6\"\n") <> (drawLTS' [] [] "1" t) <> (text "}"))
drawLTS' fs args n (Free x) = (text ("\"" ++ n ++ "\"[shape=box label = \"\"]\n")) <> (text ("\"" ++ (n ++ "0") ++ "\"[shape=box label = \"\"]\n"))
                              <> (text ("\"" ++ n ++ "\"->\"" ++ (n ++ "0") ++ "\"[label = \"" ++ x ++ "\"]\n"))
                              <> (hcat (map (\(i,t) -> (drawLTS' fs [] (n++(show i)) t) <> (text ("\"" ++ n ++ "\"->\"" ++ (n++(show i)) ++ "\"[label = \"#" ++ (show i) ++ "\"]\n"))) (zip [1..] args)))
drawLTS' fs args n (Bound i) = (text ("\"" ++ n ++ "\"[shape=box label = \"\"]\n")) <> (text ("\"" ++ (n ++ "0") ++ "\"[shape=box label = \"\"]\n"))
                               <> (text ("\"" ++ n ++ "\"->\"" ++ (n ++ "0") ++ "\"[label = \"" ++ (show i) ++ "\"]\n"))
                               <> (hcat (map (\(i,t) -> (drawLTS' fs [] (n++(show i)) t) <> (text ("\"" ++ n ++ "\"->\"" ++ (n++(show i)) ++ "\"[label = \"#" ++ (show i) ++ "\"]\n"))) (zip [1..] args)))
drawLTS' fs args n (Lambda x t) = let x' = renamevar (free t) x
                                  in  (text ("\"" ++ n ++ "\"[shape=box label=\"\"]\n")) <> (drawLTS' fs args (n ++ "1") (subst 0 (Free x) t)) <> (text ("\"" ++ n ++ "\"->\"" ++ (n ++ "1") ++ "\"[label = \\" ++ x' ++ "\"]\n"))
drawLTS' fs args n (Con c ts) = (text ("\"" ++ n ++ "\"[shape=box label = \"\"]\n")) <> (text ("\"" ++ (n ++ "0") ++ "\"[shape=box label = \"\"]\n"))
                                <> (text ("\"" ++ n ++ "\"->\"" ++ (n ++ "0") ++ "\"[label = \"" ++ c ++ "\"]\n"))
                                <> (hcat (map (\(i,t) -> (drawLTS' fs args (n++(show i)) t) <> (text ("\"" ++ n ++ "\"->\"" ++ (n++(show i)) ++ "\"[label = \"#" ++ (show i) ++ "\"]\n"))) (zip [1..] ts)))
drawLTS' fs args n (Apply t u) = drawLTS' fs (u:args) n t
drawLTS' fs args n (Case t bs) = (text ("\"" ++ n ++ "\"[shape=box label=\"\"]\n")) <> (drawLTS' fs args (n ++ "0") t)
                                 <> (text ("\"" ++ n ++ "\"->\"" ++ (n ++ "0") ++ "\"[label = \"case\"]\n"))
                                 <> (hcat (map (\(i,(c,xs,t)) -> let xs' = map (renamevar (free t)) xs
                                                                     t' = foldr (\x t -> subst 0 (Free x) t) t xs'
                                                                 in  (drawLTS' fs args (n++(show i)) t') <> (text ("\"" ++ n ++ "\"->\"" ++ (n++(show i)) ++ "\"[label = \"" ++ (show (Con c (map Free xs))) ++ "\"]\n"))) (zip [1..] bs)))
drawLTS' fs args n (Let x t u) = let x' = renamevar (free u) x
                                 in  (text ("\"" ++ n ++ "\"[shape=box label=\"\"]\n")) <> (drawLTS' fs args (n ++ "1") t) <> (drawLTS' fs args (n ++ "2") (subst 0 (Free x') u))
                                     <> (text ("\"" ++ n ++ "\"->\"" ++ (n ++ "1") ++ "\"[shape=box label = \"@\"]\n")) <> (text ("\"" ++ n ++ "\"->\"" ++ (n ++ "2") ++ "\"[shape=box label = \"\\" ++ x' ++ "\"]\n"))
drawLTS' fs args n (Unfold f t u) = case (find (\(f',n') -> f==f') fs) of
                                       Just (f',n') -> error ("Function not folded: " ++ f)
                                       Nothing -> (text ("\"" ++ n ++ "\"[shape=box label=\"\"]\n")) <> (drawLTS' ((f,n):fs) args (n ++ "1") t)
                                                  <> (text ("\"" ++ n ++ "\"->\"" ++ (n ++ "1") ++ "\"[label = \"\"]\n"))
drawLTS' fs args n (Fold f t) = case (find (\(f',n') -> f==f') fs) of
                                   Just (f',n') -> (text ("\"" ++ n ++ "\"[shape=box label=\"\"]\n")) <> (text ("\"" ++ n ++ "\"->\"" ++ n' ++ "\"[style=dotted]"))
                                   Nothing -> error ("No matching function for fold: " ++ f)
-}
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

potDef = emptyDef
         { commentStart    = "/*"
         , commentEnd      = "*/"
         , commentLine     = "--"
         , nestedComments  = True
         , identStart      = lower
         , identLetter     = do letter <|> oneOf "_'"
         , reservedNames   = ["case","of","where","ALL","EX","ANY"]
         , reservedOpNames = ["~","/\\","\\/","<=>","=>"]
         , caseSensitive   = True
         }

lexer = T.makeTokenParser potDef

symbol     = T.symbol lexer
bracks     = T.parens lexer
semic      = T.semi lexer
comm       = T.comma lexer
identifier = T.identifier lexer
reserved   = T.reserved lexer
reservedOp = T.reservedOp lexer
natural    = T.natural lexer

con = do
      c <- upper
      cs <- many letter
      return (c:cs)

makeProg t fs = let (fn,_) = unzip fs
                in  Program (makeFuns fn t) (map (\(f,t)->(f,makeFuns fn t)) fs)

makeFuns fn (Free x) = if x `elem` fn then Fun x else Free x
makeFuns fn (Bound i ) = Bound i
makeFuns fn (Lambda x t) = Lambda x (makeFuns fn t)
makeFuns fn (Con c ts) = Con c (map (makeFuns fn) ts)
makeFuns fn (Apply t u) = Apply (makeFuns fn t) (makeFuns fn u)
makeFuns fn (Fun f) = Fun f
makeFuns fn (Case t bs) = Case (makeFuns fn t) (map (\(c,xs,t) -> (c,xs,makeFuns fn t)) bs)
makeFuns fn (Let x t u) = Let x (makeFuns fn t) (makeFuns fn u)
makeFuns fn (Unfold f t u) = Unfold f (makeFuns fn t) (makeFuns fn u)
makeFuns fn (Fold f t) = Fold f (makeFuns fn t)

prog = do
       e <- expr
       fs <-     do
                 reserved "where"
                 fs <- many1 fundef
                 return fs
             <|> do
                 spaces
                 return []
       return (makeProg e fs)

fundef = do
         f <- identifier
         symbol "="
         e <- expr
         semic
         return (f,e)

expr = buildExpressionParser prec term

prec = [ [ unop "~" (Fun "not")],
         [ op "/\\" (Fun "and") AssocRight ],
         [ op "\\/" (Fun "or") AssocRight ],
         [ op "<=>" (Fun "iff") AssocRight ],
         [ op "=>" (Fun "implies") AssocRight ]
       ]
       where
       op o t assoc   = Infix (do
                               reservedOp o
                               return (\x y -> Apply (Apply t x) y)
                              ) assoc
       unop o t       = Prefix (do
                                reservedOp o
                                return (\x -> Apply t x)
                               )

term =     do
           f <- atom
           as <- many atom
           return (foldl Apply f as)
       <|> do
           symbol "\\"
           xs <- many1 identifier
           symbol "."
           e <- expr
           return (foldr (\x t->Lambda x (abstract 0 x t)) e xs)
       <|> do
           reserved "case"
           e <- expr
           reserved "of"
           bs <- sepBy1 branch (symbol "|")
           return (Case e bs)

atom =     do
           x <- identifier
           return (Free x)
       <|> do
           c <- con
           es <-     do
                     es <- bracks (sepBy1 expr comm)
                     return es
                 <|> do
                     spaces
                     return []
           return (Con c es)
       <|> do
           n <- natural
           return (nat2con n)
       <|> do
           symbol "["
           ts <- sepBy expr comm
           symbol "]"
           return (list2con ts)
       <|> do
           e <- bracks expr
           return e

branch = do
         c <- con
         xs <-     do
                   xs <- bracks (sepBy1 identifier comm)
                   return xs
               <|> do
                   spaces
                   return []
         symbol "->"
         e <- expr
         return (c,xs,foldl (\t x -> abstract 0 x t) e xs)

parseExpr input = parse expr "(ERROR)" input

parseProg input = parse prog "(ERROR)" input



