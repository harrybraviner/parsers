module LogiLang where

data Boolean = True | False deriving (Show, Eq)
data Term =   BooleanTerm Boolean
            | IfTerm Term Term Term
            | AndTerm Term Term deriving (Show, Eq)


isValue :: Term -> Bool
isValue x = case x of
                BooleanTerm _ -> Prelude.True
                IfTerm _ _ _  -> Prelude.False
                AndTerm _ _   -> Prelude.False

reduce :: Term -> Term
reduce (IfTerm (BooleanTerm LogiLang.True)  x _) = x
reduce (IfTerm (BooleanTerm LogiLang.False) _ y) = y
reduce (IfTerm t1 t2 t3)  = IfTerm (reduce t1) t2 t3
reduce (AndTerm (BooleanTerm x) (BooleanTerm y)) =
    if (x == LogiLang.True) && (y == LogiLang.True) then BooleanTerm LogiLang.True
                                                    else BooleanTerm LogiLang.False
reduce (AndTerm (BooleanTerm x) t2) =
    AndTerm (BooleanTerm x) (reduce t2)
reduce (AndTerm t1 t2) =
    AndTerm (reduce t1) t2


fullyReduce :: Term -> Term
fullyReduce t =
    if isValue t then t
                 else fullyReduce (reduce t)

