data Term = VarP String
            | Func String [Term]
    deriving (Show)

data Pred = Pr String [Term]
            | Neg Pred
            | Conj Pred Pred
            | Disj Pred Pred
            | Imp Pred Pred
            | Eqv Pred Pred
            | All String Pred
            | Ex String Pred
    deriving (Show)

type Sub = (String,Term)

--Definir una función que devuelva las variables libres de una formula.
devolverVariableslLibres :: Pred -> [VarP]
devolverVariableslLibres (Pr String lt) =
devolverVariableslLibres (Neg Pred)
devolverVariableslLibres (Conj Pred Pred)
devolverVariableslLibres (Disj Pred Pred)
devolverVariableslLibres (Imp Pred Pred)
devolverVariableslLibres (Eqv Pred Pred)
devolverVariableslLibres 

--Definir una función que devuelva las variables ligadas de una formula.
devolverVariableslLigadas :: Pred -> [VarP]
devolverVariableslLigadas (Pr String lt)
devolverVariableslLigadas (Neg Pred)
devolverVariableslLigadas (Conj Pred Pred)
devolverVariableslLigadas (Disj Pred Pred)
devolverVariableslLigadas (Imp Pred Pred)
devolverVariableslLigadas 
devolverVariableslLigadas
devolverVariableslLigadas


--Definir una función que aplique la sustitución sobre términos.
aplicaSustitucionSobreTerminos :: Term -> Sub -> Term
aplicaSustitucionSobreTerminos (VarP s) (c,t) = if (s==c)
                                                then t 
                                                else (VarP s)
aplicaSustitucionSobreTerminos (Func s l) (c,t) = Func s (auxiliarListaTerminos l (c,t))  

auxiliarListaTerminos :: [Term] -> Sub -> [Term]
auxiliarListaTerminos [] _ = []
auxiliarListaTerminos (x:xs) (c,t) = [aplicaSustitucionSobreTerminos x (c,t)] ++ (auxiliarListaTerminos xs (c,t)) 

--Definir una funcion que tome una formula φ y devuelva una formula α-equivalente a φ
--formulaAlfaEquivalente :: Pred -> Pred

--Definir una función que aplique la sustitución sobre formulas.
--aplicaSustitucionSobreFormulas :: Pred -> Sub -> Pred
