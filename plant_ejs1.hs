import Data.List
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

var_libres :: Pred -> [String]
var_libres (Pr s lt) = listaVariables lt
var_libres (Neg p) = (var_libres p) 
var_libres (Conj p q) = union (var_libres p) (var_libres q)
var_libres (Disj p q) = union (var_libres p) (var_libres q)
var_libres (Imp p q) = union (var_libres p) (var_libres q)
var_libres (Eqv p q) = union (var_libres p) (var_libres q)
var_libres (All s p) = [x|x<-(var_libres p),x/=s]
var_libres (Ex s p) = [x|x<-(var_libres p),x/=s]
--var_libres p = undefined 

listaVariables :: [Term] -> [String]
listaVariables [] = []
listaVariables (x:xs) = union (varTer x) (listaVariables xs)

varTer :: Term -> [String]
varTer (VarP s) = [s]
varTer (Func s lt) = listaVariables lt 

var_lig  :: Pred -> [String]
var_lig (Pr s lt) = []
var_lig (Neg p) = var_lig p
var_lig (Conj p q) = union (var_lig p) (var_lig q)
var_lig (Disj p q) = union (var_lig p) (var_lig q)
var_lig (Imp p q) = union (var_lig p) (var_lig q)
var_lig (Eqv p q) = union (var_lig p) (var_lig q)
var_lig (All s p) = union [s] (var_lig p)
var_lig (Ex s p) = union [s] (var_lig p)
--var_lig p = undefined 

alfa_eq :: Pred -> String -> Pred
alfa_eq (Pr s lt) st = (Pr s lt)
alfa_eq (Neg p) st = Neg p
alfa_eq (Conj p q) st = Conj p q 
alfa_eq (Disj p q) st = Disj p q
alfa_eq (Imp p q) st = Imp p q
alfa_eq (Eqv p q) st = Eqv p q
alfa_eq (All s p) st = All st (cambVarLig p s st)
alfa_eq (Ex s p) st = Ex st (cambVarLig p s st) 
--alfa_eq p s = p 

cambVarLig :: Pred -> String -> String -> Pred
cambVarLig (Pr p lt) s st = Pr p (auxCambial lt s st)
cambVarLig (Neg p) s st = Neg (cambVarLig p s st)
cambVarLig (Conj p q) s st = Conj (cambVarLig p s st) (cambVarLig q s st)
cambVarLig (Disj p q) s st = Disj (cambVarLig p s st) (cambVarLig q s st)
cambVarLig (Imp p q) s st = Imp (cambVarLig p s st) (cambVarLig q s st)
cambVarLig (Eqv p q) s st = Eqv (cambVarLig p s st) (cambVarLig q s st)
cambVarLig (All r p) s st = if(r==s) 
                            then alfa_eq (All r p) st
                            else (All r (cambVarLig p s st))
cambVarLig (Ex r p) s st = if(r==s) 
                           then alfa_eq (Ex r p) st
                           else Ex r (cambVarLig p s st)

auxCambial :: [Term] -> String -> String -> [Term]
auxCambial [] _ _ = []
auxCambial (x:xs) s st = (auxCambiat x s st) ++ (auxCambial xs s st)

auxCambiat :: Term -> String -> String -> [Term]
auxCambiat (VarP p) s st = if(p == s)
                           then [VarP st]
                           else [VarP p]
auxCambiat (Func p lt) s st = [Func p (auxCambial lt  s st)]

sust_term :: Term -> Sub -> Term
sust_term (VarP s) (c,t) = if(s==c)
                           then t
                           else (VarP s)
sust_term (Func s l) (c,t) = Func s (auxiliarListaTerminos l (c,t))                           
--sust_term p s = undefined 

auxiliarListaTerminos :: [Term] -> Sub -> [Term]
auxiliarListaTerminos [] _ = []
auxiliarListaTerminos (x:xs) (c,t) = [sust_term x (c,t)] ++ (auxiliarListaTerminos xs (c,t))

sust_form :: Pred -> Sub -> Pred
sust_form (Pr s lt) (st,t) = (Pr s (auxiliarListaTerminos lt (st,t)))
sust_form (Neg p) (st,t) = Neg (sust_form p (st,t))
sust_form (Conj p q) (st,t) = Conj (sust_form p (st,t)) (sust_form q (st,t))
sust_form (Disj p q) (st,t) = Disj (sust_form p (st,t)) (sust_form q (st,t))
sust_form (Imp p q) (st,t) = Imp (sust_form p (st,t)) (sust_form q (st,t))
sust_form (Eqv p q) (st,t) = Eqv (sust_form p (st,t)) (sust_form q (st,t))
sust_form (All s p) (st,t) = if(s==st)
                             then (All s p)
                             else if(distintosTerminos s t)
                                  then All s (sust_form p (st,t))
                                  else (sust_form (alfa_eq (All s p) (generaVar s (length s)) ) (st,t) )
sust_form (Ex s p) (st,t) = if(s==st)
                            then (Ex s p)
                            else if(distintosTerminos s t)
                                then Ex s (sust_form p (st,t))
                                else (sust_form (alfa_eq (Ex s p) (generaVar s (length s)) ) (st,t) )
--sust_form p s = undefined 

generaVar :: String -> Int -> String
generaVar s n = s++(show n)

distintosTerminos :: String -> Term -> Bool
distintosTerminos s (VarP st) = (st/=s)
distintosTerminos s (Func st lt) = (auxDistintos s lt)

auxDistintos :: String -> [Term] -> Bool
auxDistintos _ [] = True
auxDistintos s (x:xs) = (distintosTerminos s x) && (auxDistintos s xs)

--sustForAux ::  