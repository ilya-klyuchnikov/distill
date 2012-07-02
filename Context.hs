module Context where

import Term

data Context = EmptyCtx
             | AppCtx Context Term
             | CaseCtx Context [Branch] deriving Show

place e EmptyCtx = e
place e (AppCtx con t) = place (App e t) con
place e (CaseCtx con bs) = place (Case e bs) con