module Context where

import Expr

data Context = EmptyCtx
             | AppCtx Context Expr
             | CaseCtx Context [Branch] deriving Show

place e EmptyCtx = e
place e (AppCtx con t) = place (App e t) con
place e (CaseCtx con bs) = place (Case e bs) con

shiftCtx i d EmptyCtx = EmptyCtx
shiftCtx i d (AppCtx con t) = AppCtx (shiftCtx i d con) (shift i d t)
shiftCtx i d (CaseCtx con bs) = CaseCtx (shiftCtx i d con) (map (\(Branch c xs t) -> (Branch c xs $ shift i (d + length xs) t)) bs)