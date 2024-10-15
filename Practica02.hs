module Practica02 where

--PRACTICA LÓGICA PROPOSICIONAL.

-- Definición del tipo LProp
data LProp = PTrue | PFalse | Var Nombre | Neg LProp | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp deriving Eq

type Nombre = String

type Asignacion = (String, Bool)

instance Show LProp where
    show PTrue = "True"
    show PFalse = "False"
    show (Var p) = p
    show (Neg p) = "¬("++ show p ++ ")" 
    show (Conj p q) = show p ++ "^" ++ show q
    show (Disy p q) = show p ++ "v" ++ show q
    show (Impl p q) = show p ++ "->" ++ show q
    show (Syss p q) = show p ++ "<->" ++ show q


--1. vars: Toma como argumento una fórmula y regresa las variables en la formula.

-- Función que elimina los duplicados de una lista
-- La función toma como entrada una lista y regresa otra lista
-- Ej. ["p","p"] -> ["p"]
elimina_duplicado :: Eq a => [a] -> [a]
elimina_duplicado [] = []
elimina_duplicado (x:xs)    | elem x xs = elimina_duplicado xs
                            | otherwise = x : elimina_duplicado xs

-- Función que regresa todas las variables de una proposición.
-- Toma como entrada una proposición y regresa una lista con las variables
-- Ejemplo: p v q -> ["p","q"]
vars :: LProp -> [Nombre]
vars PTrue = []
vars PFalse = []
vars (Var n) = [n]
vars (Neg n) = vars n
vars (Conj p q) = elimina_duplicado(vars (p) ++ vars (q)) 
vars (Disy p q) = elimina_duplicado(vars (p) ++ vars (q)) 
vars (Impl p q) = elimina_duplicado(vars (p) ++ vars (q))
vars (Syss p q) = elimina_duplicado(vars (p) ++ vars (q))

--2. deMorgan: Toma una fórmula proposicional y regresa su valor aplicando las leyes de De Morgan. Por ejemplo: deMorgan ¬(p ∧ q) regresa (¬p ∨ ¬q).
--Si nose aplica la ley, regresa la fórmula original. 

-- Toma como entrada una proposición y regresa la proposición si se aplica la ley
-- Ejemplo: ¬ (p v q) -> ¬p ^ ¬q
deMorgan :: LProp -> LProp
deMorgan PTrue = PTrue
deMorgan PFalse = PFalse
deMorgan (Neg PTrue) = PFalse
deMorgan (Neg PFalse) = PTrue
deMorgan (Var n) = (Var n)
deMorgan (Neg (Disy a b)) = Conj (deMorgan (Neg a)) (deMorgan (Neg b))
deMorgan (Neg (Conj a b)) = Disy (deMorgan (Neg a)) (deMorgan (Neg b))
deMorgan (Neg n) = Neg (deMorgan n)
deMorgan (Conj a b) = Conj (deMorgan a) (deMorgan b)
deMorgan (Disy a b) = Disy (deMorgan a) (deMorgan b)
deMorgan (Impl a b) = Impl (deMorgan a) (deMorgan b)
deMorgan (Syss a b) = Syss (deMorgan a) (deMorgan b)

--3. equiv op: Dada una formula con implicacion → regresa su equivalente con conectores basicos. Por ejemplo, para p → q regresara el valor ¬p ∨ q.
-- Recibe una proposición y regresa otra proposición sin implicaciones ni doble implicaciones.
-- Ejemplo: p → q -> ¬p v q
equiv_op :: LProp -> LProp
equiv_op PTrue = PTrue
equiv_op PFalse = PFalse
equiv_op (Var p) = Var p
equiv_op (Neg p) = Neg (equiv_op p)
equiv_op (Impl p q) = (Disy (Neg (equiv_op p)) (equiv_op q))
equiv_op (Conj p q) = Conj (equiv_op p) (equiv_op q)
equiv_op (Disy p q) = Disy (equiv_op p) (equiv_op q)
equiv_op (Syss p q) = Conj (Disy (Neg (equiv_op p)) (equiv_op q)) (Disy (Neg (equiv_op q)) (equiv_op p))
--4.dobleNeg: Dada una formula con doble negacion elimina esta. Por ejemplo, para ¬(¬p) regresar p.
-- Recibe una proposición y le quita las dobles negaciones.
-- Ejemplo: ¬(¬p) -> p
dobleNeg :: LProp -> LProp
dobleNeg PTrue = PTrue
dobleNeg PFalse = PFalse
dobleNeg (Var p) = Var p
dobleNeg (Neg(Neg p)) = dobleNeg(p)
dobleNeg (Neg p) = Neg (dobleNeg p)
dobleNeg (Conj p q) = Conj (dobleNeg p) (dobleNeg q)
dobleNeg (Disy p q) = Disy (dobleNeg p) (dobleNeg q)
dobleNeg (Impl p q) = Impl (dobleNeg p) (dobleNeg q)
dobleNeg (Syss p q) = Syss (dobleNeg p) (dobleNeg q)
--5. num_conectivos: Funcion recursiva para contar el número de conectivos lógicos de una formula.
-- Recibe una proposición y regresa un entero con el número de conectivos lógicos.
-- Ejemplo: p v q -> 1
num_conectivos :: LProp -> Int
num_conectivos PTrue = 0
num_conectivos PFalse = 0
num_conectivos (Var _) = 0
num_conectivos (Neg p) = 1 + num_conectivos p
num_conectivos (Conj p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Disy p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Impl p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Syss p q) = 1 + num_conectivos p + num_conectivos q

--6. num_variables: Funcion recursiva para contar el número de variables de una fórmula.
-- Recibe una proposición y regresa el número de variables sin duplicar de la fórmula.
-- Ejemplo: p v q -> 2

num_variables :: LProp -> Int
num_variables = length . elimina_duplicado . collect_vars
  where
    collect_vars :: LProp -> [String]
    collect_vars PTrue = []
    collect_vars PFalse = []
    collect_vars (Var p) = [p]
    collect_vars (Neg p) = collect_vars p
    collect_vars (Conj p q) = collect_vars p ++ collect_vars q
    collect_vars (Disy p q) = collect_vars p ++ collect_vars q
    collect_vars (Impl p q) = collect_vars p ++ collect_vars q
    collect_vars (Syss p q) = collect_vars p ++ collect_vars q

--7. profundidad: Funcion recursiva que regresa la profundidad de una fórmula lógica.
-- Recibe una proposición y regresa un entero.
-- Ejemplo: ¬p v (p ^ q) -> 2
profundidad :: LProp -> Integer
profundidad (PTrue) = 0
profundidad (PFalse) = 0
profundidad (Var p) = 0
profundidad (Neg p) = 1 + profundidad(p)
profundidad (Conj q p) = 1 + max(profundidad q)(profundidad p)
profundidad (Disy q p) = 1 + max(profundidad q)(profundidad p)
profundidad (Impl q p) = 1 + max(profundidad q)(profundidad p)
profundidad (Syss q p) = 1 + max(profundidad q)(profundidad p)
--8.interpretación: Regresa los valores de verdad, True o False, segun una asignación. 
-- Toma como argumentos una fórmula y una asignación de las variables.
-- Por ejemplo: interpretación (p∧q) [(”p”, True), (”q”, False)] regresará True.
-- Recibe una proposición y una lista con los valores de las proposiciones.

interpretación :: LProp -> [Asignacion] -> Bool
interpretación PTrue _ = True
interpretación PFalse _ = False
interpretación (Var _) [] = error "error"
interpretación (Var p) ((x,val):xs) = if p == x then val else interpretación (Var p) xs 
interpretación (Neg p) (x:xs) = not (interpretación p (x:xs))
interpretación (Conj p q) (x:xs) = interpretación p (x:xs) == True && interpretación q (x:xs) == True
interpretación (Disy p q) (x:xs) = interpretación p (x:xs) == True || interpretación q (x:xs) == True
interpretación (Impl p q) (x:xs) = interpretación (Neg p) (x:xs) == True || interpretación q (x:xs) == True
interpretación (Syss p q) (x:xs) = interpretación p (x:xs) == interpretación q (x:xs)
