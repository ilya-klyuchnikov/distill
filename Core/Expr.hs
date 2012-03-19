module Core.Expr where

import qualified Language.Haskell.Syntax as L

data Program = Program Expr [Function]

data Expr = Var String
          | Con String [Expr]
          | Func String
          | Lambda String Expr
          | App Expr Expr
          | Case Expr [Branch]
          | LetRec [Function] Expr
          | Lit L.HsLiteral
          
data Pattern = Pattern String [String]

type Branch = (Pattern, Expr)
type Function = (String, Expr)
