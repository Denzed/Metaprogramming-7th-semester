module Examples where

import Base

zero = ("zero", Cons "Z" [])
one  = ("one",  Cons "S" [Cons "Z" []])
succ = ("succ", Lmb "x" $ Cons "S" [(Var "x")])
add  = ("add", Lmb "x" $ Lmb "y" $ Case (Var "x") [
    (Uncons "S" ["x'"], App (App (Func "add") (Var "x'")) (App (Func "succ") (Var "y"))),
    (Uncons "Z" []    , Var "y")
    ])

addProgram = Prog (App (App (Func "add") (Var "one")) (Var "one")) [one, Examples.succ, add]
