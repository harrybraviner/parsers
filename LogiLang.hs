module LogiLang where

data Boolean = True | False deriving Show
data Term = BooleanTerm Boolean | IfTerm Term Term Term deriving Show

isValue :: Term -> Bool
isValue x = case x of
                BooleanTerm _ -> Prelude.True
                IfTerm _ _ _  -> Prelude.False

reduce :: Term -> Term
reduce (IfTerm (BooleanTerm LogiLang.True)  x _) = x
reduce (IfTerm (BooleanTerm LogiLang.False) _ y) = y
reduce (IfTerm t1 t2 t3)  = IfTerm (reduce t1) t2 t3

fullyReduce :: Term -> Term
fullyReduce t =
    if isValue t then t
                 else fullyReduce (reduce t)

