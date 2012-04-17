module Core.Pretty where

import Core.Term
import Text.PrettyPrint.HughesPJ
import Language.Haskell.Pretty (prettyPrint)

class Pretty a where
    pretty :: a -> Doc
    prettyShow :: a -> String
    prettyShow = render . pretty

instance Pretty Program where
    pretty (Program main funcs) = vcat $ punctuate (text "\n\n") $ map prettyFunction (("main", main):funcs)

instance Pretty Term where
    pretty (Free v) = text v
    pretty (Bound i) = int i
    pretty (Con "List" []) = text "[]"
    pretty (Con "List" es)
     | null es = text "[]"
     | otherwise = parens $ hcat $ punctuate colon $ map pretty es
    pretty (Con "(:)" es) = error "here" --parens $ hcat (punctuate colon $ map pretty es)
    pretty con@(Con c es) 
     | isNat con = int $ con2nat con
     | isList con = brackets $ hcat (punctuate comma (map pretty $ con2list con))
     | otherwise = text c <+> hcat (punctuate space $ map pretty es)
    pretty (Lit l) = text $ prettyPrint l
    pretty (Fun f) = text f
    pretty (Apply e@(Lambda {}) e') = parens (pretty e) <+> pretty e'
    pretty (Apply e e') = parens (pretty e <+> parens (pretty e'))
    pretty (Case e b) = hang (text "case" <+> pretty e <+> text "of") 1 $ vcat $ map prettyBranch b
    pretty (Lambda v e) = let (xs,u) = stripLambda e
                          in  (text "\\") <> (hsep (map text xs)) <> (text "->") <> (prettyTerm u)
    pretty (Typed e t) = parens (parens (pretty e) <+> text "::" <> text (prettyPrint t))

instance Show Term where
    show = prettyShow
    
instance Show Program where
    show = prettyShow

prettyBranch (c,[],t) = (text c) <+> (text "->") <+> (pretty t)
prettyBranch (c,xs,t) = let xs' = map (renamevar (free t)) xs
                            t' = foldr (\x t->subst 0 (Free x) t) t xs'
                        in  (text c) <> (parens (hcat (punctuate comma (map text xs')))) <+> (text "->") <+> (pretty t') $$ empty
{-
prettyBranch ("(:)", es, e) = (parens $ hcat $ punctuate colon $ map text es) <+> text "->" <+> pretty e
prettyBranch ("[]", es, e) = text "[]" <+> text "->" <+> pretty e
prettyBranch ("Cons", es, e) = (parens $ hcat $ punctuate colon $ map text es) <+> text "->" <+> pretty e
prettyBranch ("Nil", [], e) = text "[]" <+> text "->" <+> pretty e
prettyBranch (c, es, e) = (text c <+> hcat (punctuate space $ map text es)) <+> text "->" <+> pretty e
-}
prettyFunction :: (String, Term) -> Doc
prettyFunction (name, body) = text name <+> text "=" <+> pretty body