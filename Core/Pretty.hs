module Core.Pretty where

import Core.Expr
import Text.PrettyPrint.HughesPJ

class Pretty a where
    pretty :: a -> Doc
    prettyShow :: a -> String
    prettyShow = render . pretty

instance Pretty Expr where
    pretty (Var v) = text v
    pretty (Con "List" []) = text "[]"
    pretty (Con "List" es)
     | null es = text "[]"
     | otherwise = parens $ hcat $ punctuate colon $ map pretty es
    pretty (Con c es) = text c <+> hcat (punctuate space $ map pretty es)
    pretty (Lit l) = text $ show l
    pretty (Func f) = text f
    pretty (App e@(Var v) e'''@(App e' e''))
     | v `elem` ["$", ">>=", "=<<"] = pretty e' <+> text v <+> pretty e''
     | otherwise = pretty e <+> pretty e'''
    pretty (App e@(Lambda {}) e') = parens (pretty e) <+> pretty e'
    pretty (App e e') = pretty e <+> pretty e'
    pretty (Case e b) = hang (text "case" <+> pretty e <+> text "of") 1 $ vcat $ map (\(p, e) -> pretty p <+> text "->" <+> pretty e) b
    pretty (Lambda v e) = text "\\" <> text v <+> text "->" <+> pretty e
    pretty (LetRec [] e) = pretty e

instance Pretty Pattern where
    pretty (Pattern "(:)" es) = parens $ hcat $ punctuate colon $ map text es
    pretty (Pattern "Cons" es) = parens $ hcat $ punctuate colon $ map text es
    pretty (Pattern "Nil" []) = text "[]"
    pretty (Pattern c as) = text c <+> hcat (punctuate space $ map text as)

instance Show Expr where
    show = prettyShow

instance Show Pattern where
    show = prettyShow


prettyFunction :: (String, Expr) -> Doc
prettyFunction (name, body) = text name <+> text "=" <+> pretty body