module Core.Pretty where

import Core.Expr
import Text.PrettyPrint.HughesPJ
import Language.Haskell.Pretty (prettyPrint)

class Pretty a where
    pretty :: a -> Doc
    prettyShow :: a -> String
    prettyShow = render . pretty

instance Pretty Expr where
    pretty (Var v) = text v
    pretty (Bound i) = int i
    pretty (Con "List" []) = text "[]"
    pretty (Con "List" es)
     | null es = text "[]"
     | otherwise = parens $ hcat $ punctuate colon $ map pretty es
    pretty (Con "(:)" es) = parens $ hcat (punctuate colon $ map pretty es)
    pretty (Con c (e:e':[]))
     | length c == 1 = pretty e <+> text c <+> pretty e'
    pretty con@(Con c es) 
     | isNat con = int $ con2nat con
     | isList con = brackets $ hcat (punctuate comma (map pretty $ con2list con))
     | otherwise = text c <+> hcat (punctuate space $ map pretty es)
    pretty (Lit l) = text $ prettyPrint l
    pretty (Func f) = text f
    pretty (App e@(Var v) e'''@(App e' e''))
     | v `elem` ["$", ">>=", "=<<"] = pretty e' <+> text v <+> pretty e''
     | otherwise = pretty e <+> pretty e'''
    pretty (App e@(Lambda {}) e') = parens (pretty e) <+> pretty e'
    pretty (App e e') = parens (pretty e <+> parens (pretty e'))
    pretty (InfixApp e "(:)" e') = parens $ pretty e <> text ":" <> pretty e'
    pretty (InfixApp e c e') = parens $ pretty e <> text c <> pretty e'
    pretty (Case e b) = hang (text "case" <+> pretty e <+> text "of") 1 $ vcat $ map (\(p, e) -> pretty p <+> text "->" <+> pretty e) b
    pretty (Lambda v e) = text "\\" <> text v <+> text "->" <+> pretty e
    pretty (Typed e t) = parens (parens (pretty e) <+> text "::" <> text (prettyPrint t))

instance Pretty Pattern where
    pretty (Pattern "(:)" es) = parens $ hcat $ punctuate colon $ map text es
    pretty (Pattern "[]" []) = text "[]"
    pretty (Pattern "Cons" es) = parens $ hcat $ punctuate colon $ map text es
    pretty (Pattern "Nil" []) = text "[]"
    pretty (Pattern c as) = text c <+> hcat (punctuate space $ map text as)

instance Show Expr where
    show = prettyShow

instance Show Pattern where
    show = prettyShow


prettyFunction :: (String, Expr) -> Doc
prettyFunction (name, body) = text name <+> text "=" <+> pretty body