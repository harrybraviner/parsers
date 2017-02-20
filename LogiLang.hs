module LogiLang where

data Boolean = True | False deriving (Show, Eq)
data Term =   BooleanTerm Boolean
            | IfTerm Term Term Term
            | AndTerm Term Term
            | Zero
            | Succ Term
            | Pred Term
            | IsZero Term
            | Wrong deriving (Show, Eq)


isValue :: Term -> Bool
isValue x = case x of
                BooleanTerm _ -> Prelude.True
                Zero          -> Prelude.True
                Wrong         -> Prelude.True
                Succ x        -> isNumeric x
                _             -> Prelude.False

isNumeric :: Term -> Bool
isNumeric x = case x of
                Zero   -> Prelude.True
                Succ x -> isNumeric x
                _      -> Prelude.False

isBool :: Term -> Bool
isBool x = case x of
            BooleanTerm LogiLang.True  -> Prelude.True
            BooleanTerm LogiLang.False -> Prelude.True
            _                          -> Prelude.False

isBadNat :: Term -> Bool
isBadNat Wrong                        = Prelude.True
isBadNat (BooleanTerm LogiLang.True ) = Prelude.True
isBadNat (BooleanTerm LogiLang.False) = Prelude.True
isBadNat _                            = Prelude.False

isBadBool :: Term -> Bool
isBadBool Wrong = Prelude.True
isBadBool x     = isNumeric x

reduce :: Term -> Term

reduce (IfTerm (BooleanTerm LogiLang.True)  x _) = x
reduce (IfTerm (BooleanTerm LogiLang.False) _ y) = y
reduce (IfTerm t1 t2 t3) =
    if isBadBool t1 then Wrong
                    else IfTerm (reduce t1) t2 t3

reduce (AndTerm (BooleanTerm x) (BooleanTerm y)) =
    if (x == LogiLang.True) && (y == LogiLang.True) then BooleanTerm LogiLang.True
                                                    else BooleanTerm LogiLang.False
reduce (AndTerm (BooleanTerm x) t2) =
    if isBadBool t2 then Wrong
                    else AndTerm (BooleanTerm x) (reduce t2)
reduce (AndTerm t1 t2) =
    if isBadBool t1 then Wrong
                    else AndTerm (reduce t1) t2

reduce (Pred Zero) = Zero
reduce (Pred (Succ x)) =
    if isNumeric x then x
                   else Wrong
reduce (Pred t) =
    if isBadNat t then Wrong
                  else Pred (reduce t)

reduce (Succ t) =
    if isBadNat t then Wrong
                  else Succ (reduce t)

reduce (IsZero Zero) = (BooleanTerm LogiLang.True)
reduce (IsZero (Succ x)) =
    if isNumeric x then (BooleanTerm LogiLang.False)  -- Not strictly true. Should require that x is a numeric value.
                   else if isBadNat x then Wrong
                                      else IsZero (reduce (Succ x))
reduce (IsZero t) =
    if isBadNat t then Wrong
                  else IsZero (reduce t)

fullyReduce :: Term -> Term
fullyReduce t =
    if isValue t then t
                 else fullyReduce (reduce t)

