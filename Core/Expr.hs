module Core.Expr where

data Prog = Prog Expr [Function]    -- Program (body of main, with environment functions)

instance Eq Prog where
    (Prog m fs) == (Prog m' fs') = m == m' && fs == fs'

data Expr = Var String                -- Variable
          | Bound Int                 -- Bound Variable
          | InfixApp String Expr Expr -- Infix Application
          | Con String [Expr]         -- Constructor
          | Func String               -- Function Call
          | Lambda String Expr        -- Lambda Abstraction
          | App Expr Expr             -- Application
          | Case Expr [Branch]        -- Case Expression
          | Let (String, Expr) Expr   -- Let Expression
          | Paren Expr                -- Parenthesized Expression
          | Lit Lit                   -- Literal Expression

instance Eq Expr where
    (Var v)         == (Var v')         = v == v'
    (Con c es)      == (Con c' es')     = c == c' && es == es'
    (Func f)        == (Func f')        = f == f'
    (Lambda v e)    == (Lambda v' e')   = v == v' && e == e'
    (App e a)       == (App e' a')      = e == e' && a == a'
    (Case e b)      == (Case e' b')     = e == e' && b == b'
    (Let b e)       == (Let b' e')      = b == b' && e == e'
    (Paren e)       == (Paren e')       = e == e'
    (Lit l)         == (Lit l')         = l == l'
    _ == _ = False

data Lit = Char Char
         | String String
         | Int Integer
         | Frac Rational
    deriving Eq

data Pattern = Pattern String [String]  -- Pattern
    deriving Eq

type Function = (String, Expr)  -- Function name & body
type Branch = (Pattern, Expr)   -- Branch