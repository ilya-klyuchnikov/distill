module Core.Pretty where

import Core.Expr
import Text.PrettyPrint.HughesPJ

class Pretty a where
	pretty :: a -> Doc
	prettyShow :: a -> String
	prettyShow = render . pretty

instance Pretty Expr where
	pretty (Var v) = text v
	pretty (Con "Nil" _) = text "[]"
	pretty (Con "Cons" es)
	 | null es = text "[]"
	 | otherwise = parens $ hcat $ punctuate colon $ map pretty es
	pretty (Con c es) = text c <+> hcat (punctuate space $ map pretty es)
	pretty (Lit l) = pretty l
	pretty (Func f) = text f
	pretty (App Not Not) = text ""
	pretty (App Not e) = pretty e
	pretty (App e Not) = pretty e
	pretty (App e@(Var v) e'''@(App e' e''))
	 | v `elem` ["$", ">>=", "=<<"] = pretty e' <+> text v <+> pretty e''
	 | otherwise = pretty e <+> pretty e'''
	pretty (App e@(Lambda {}) e') = parens (pretty e) <+> pretty e'
	pretty (App e e') = pretty e <+> pretty e'
	pretty (Case e b) = hang (text "case" <+> pretty e <+> text "of") 1 $ vcat $ map (\(p, e) -> pretty p <+> text "->" <+> pretty e) b
	pretty (Lambda v e) = text "\\" <> text v <+> text "->" <+> pretty e
	pretty (Let (v, e) e') = hang (hang (text "let") 1 $ text v <+> text "=" <+> pretty e) 0 (text "in" <+> pretty e')
	pretty (Paren e@(Paren {})) = pretty e
	pretty (Paren e@(Con {})) = pretty e
	pretty (Paren e) = parens $ pretty e
	pretty (Not) = text ""

instance Pretty Lit where
	pretty (Char c) = text $ "'" ++ [c] ++ "'"
	pretty (String s) = text $ "\"" ++ s ++ "\""
	pretty (Int i) = integer i
	pretty (Frac f) = rational f

instance Pretty Pattern where
	pretty (Pattern "(:)" es) = parens $ hcat $ punctuate colon $ map text es
	pretty (Pattern "Cons" es) = parens $ hcat $ punctuate colon $ map text es
	pretty (Pattern "Nil" []) = text "[]"
	pretty (Pattern c as) = text c <+> hcat (punctuate space $ map text as)

instance Show Prog where
    show (Prog m fs) = concatMap (\f -> render (prettyFunction f) ++ "\n\n") (("main", m):fs)

instance Show Expr where
    show = prettyShow

instance Show Lit where
    show = prettyShow

instance Show Pattern where
	show = prettyShow


prettyFunction :: (String, Expr) -> Doc
prettyFunction (name, body) = text name <+> text "=" <+> pretty body