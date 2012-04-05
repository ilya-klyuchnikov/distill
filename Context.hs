module Context where

import Core.Expr
import Core.Pretty

data Context = EmptyCtx
             | AppCtx Context Expr
             | InfixAppCtx Context String Expr
             | CaseCtx Context [(Pattern, Expr)] deriving Show

place e EmptyCtx = e
place e (AppCtx con t) = place (App e t) con
place e (InfixAppCtx con c e') = place (InfixApp e c e') con
place e (CaseCtx con bs) = place (Case e bs) con

shiftCtx i d EmptyCtx = EmptyCtx
shiftCtx i d (AppCtx con t) = AppCtx (shiftCtx i d con) (shift i d t)
shiftCtx i d (InfixAppCtx con c e) = InfixAppCtx (shiftCtx i d con) c (shift i d e)
shiftCtx i d (CaseCtx con bs) = CaseCtx (shiftCtx i d con) (map (\(Pattern c xs,t) -> (Pattern c xs,shift i (d + length xs) t)) bs)