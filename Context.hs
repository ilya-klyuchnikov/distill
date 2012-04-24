module Context where

import Core.Term

data Context = EmptyCtx
             | ApplyCtx Context Term
             | CaseCtx Context [Branch] deriving Show

place e EmptyCtx = e
place e (ApplyCtx con t) = place (Apply e t) con
place e (CaseCtx con bs) = place (Case e bs) con

shiftCtx i d EmptyCtx = EmptyCtx
shiftCtx i d (ApplyCtx con t) = ApplyCtx (shiftCtx i d con) (shift i d t)
shiftCtx i d (CaseCtx con bs) = CaseCtx (shiftCtx i d con) (map (\(Branch c xs t) -> (Branch c xs $ shift i (d + length xs) t)) bs)