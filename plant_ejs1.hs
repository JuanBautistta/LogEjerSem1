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
var_libres (VarP s) = [s]
var_libres (Neg Pred) =
var_libres (Conj Pred Pred) = 
var_libres (Disj Pred Pred) =
var_libres (Imp Pred Pred) =
var_libres (Eqv Pred Pred) =
var_libres (All String Pred) =
var_libres (Ex String Pred) =
--var_libres p = undefined 

var_lig  :: Pred -> [String]
var_lig (Pr s lt) = []
var_lig (Neg p) = var_lig p
var_lig (Conj p q) = (var_lig p) ++ (var_lig q)
var_lig (Disj p q) = (var_lig p) ++ (var_lig q)
var_lig (Imp p q) = (var_lig p) ++ (var_lig q)
var_lig (Eqv p q) = (var_lig p) ++ (var_lig q)
var_lig (All s p) = [s] ++ (var_lig p)
var_lig (Ex s p) = [s] ++ (var_lig p)
--var_lig p = undefined 

--alfa_eq :: Pred -> String -> Pred
--alfa_eq p s = p 

sust_term :: Term -> Sub -> Term
sust_term p s = undefined 

sust_form :: Pred -> Sub -> Pred
sust_form p s = undefined 
