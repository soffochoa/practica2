import Distribution.Types.Lens (PackageIdentifier)
--PRACTICA LÓGICA PROPOSICIONAL.

-- Definición del tipo LProp
data LProp = PTrue | PFalse | Var Nombre | Neg LProp | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp

type Nombre = String

type Asignacion = (String, Bool)

instance Show LProp where
    show PTrue = "True"
    show PFalse = "False"
    show (Var p) = p
    show (Neg p) = "¬"++ show p 
    show (Conj p q) = show p ++ "^" ++ show q
    show (Disy p q) = show p ++ "v" ++ show q
    show (Impl p q) = show p ++ "->" ++ show q
    show (Syss p q) = show p ++ "<->" ++ show q


--1. vars: Toma como argumento una fÓrmula y regresa las variables en la formula.

elimina_duplicado :: Eq a => [a] -> [a]
elimina_duplicado [] = []
elimina_duplicado (x:xs)    | elem x xs = elimina_duplicado xs
                            | otherwise = x : elimina_duplicado xs

vars :: LProp -> [Nombre]
vars PTrue = []
vars PFalse = []
vars (Var n) = [n]
vars (Neg n) = vars n
vars (Conj p q) = elimina_duplicado(vars (p) ++ vars (q)) 
vars (Disy p q) = elimina_duplicado(vars (p) ++ vars (q)) 
vars (Impl p q) = elimina_duplicado(vars (p) ++ vars (q))
vars (Syss p q) = elimina_duplicado(vars (p) ++ vars (q))

--2. deMorgan: Toma una f´ormula proposicional y regresa su valor aplicando las leyes de De Morgan. Por ejemplo: deMorgan ¬(p ∧ q) regresa (¬p ∨ ¬q).
--Si nose aplica la ley, regresa la f´ormula original. 
deMorgan :: LProp -> LProp
deMorgan PTrue = PTrue
deMorgan PFalse = PFalse
deMorgan (Var n) = (Var n)
deMorgan (Neg n) = Neg (deMorgan n)
deMorgan (Neg (Disy a b)) = (Conj (Neg (deMorgan (a))) (Neg (deMorgan (b))))
deMorgan (Neg (Conj a b)) = (Disy (Neg (deMorgan (a))) (Neg (deMorgan (b))))
deMorgan (Conj a b) = (Neg (Disy (deMorgan(Neg a)) (deMorgan(Neg b))))
deMorgan (Disy a b) = (Neg (Conj (deMorgan(Neg a)) (deMorgan(Neg b))))
deMorgan (Impl a b) = Impl (deMorgan a) (deMorgan b)
deMorgan (Syss a b) = Syss (deMorgan a) (deMorgan b)

--3. equiv op: Dada una formula con implicacion → regresa su equivalente con conectores basicos. Por ejemplo, para p → q regresara el valor ¬p ∨ q.
equiv_op :: LProp -> LProp
equiv_op (Impl p q) = (Disy (Neg (equiv_op p)) (equiv_op q))
equiv_op p = p
--4.dobleNeg: Dada una formula con doble negacion elimina esta. Por ejemplo, para ¬(¬p) regresar p.
--5. numConectivos: Funcion recursiva para contar el número de conectivos lógicos de una formula.
numConectivos :: LProp -> Int
numConectivos PTrue = 0
numConectivos PFalse = 0
numConectivos (Var _) = 0
numConectivos (Neg p) = 1 + numConectivos p
numConectivos (Conj p q) = 1 + numConectivos p + numConectivos q
numConectivos (Disy p q) = 1 + numConectivos p + numConectivos q
numConectivos (Impl p q) = 1 + numConectivos p + numConectivos q
numConectivos (Syss p q) = 1 + numConectivos p + numConectivos q

--6. numVariables: Funcion recursiva para contar el número de variables de una formula.
numVariables :: LProp -> Int
numVariables PTrue = 0
numVariables PFalse = 0
numVariables (Var _) = 1
numVariables (Neg p) = numVariables p
numVariables (Conj p q) = numVariables p + numVariables q
numVariables (Disy p q) = numVariables p + numVariables q
numVariables (Impl p q) = numVariables p + numVariables q
numVariables (Syss p q) = numVariables p + numVariables q
--7. profundidad: Funcion recursiva que regresa la profundidad de una f´ormula l´ogica.
--8.interpretacion: Regresa los valores de verdad, True o False, segun una asig-naci´on. Toma como argumentos una f´ormula y una asignaci´on de las variables.
--Por ejemplo: interpretacion (p∧q) [(”p”, True), (”q”, False)] regresar´a True.


interpretacion :: LProp -> [Asignacion] -> Bool
interpretacion PTrue _ = True
interpretacion PFalse _ = False
interpretacion (Var _) [] = error "error"
interpretacion (Var p) ((x,val):xs) = if p == x then val else interpretacion (Var p) xs 
interpretacion (Neg p) (x:xs) = interpretacion p (x:xs) == False
interpretacion (Conj p q) (x:xs) = interpretacion p (x:xs) == True && interpretacion q (x:xs) == True
interpretacion (Disy p q) (x:xs) = interpretacion p (x:xs) == True || interpretacion q (x:xs) == True
interpretacion (Impl p q) (x:xs) = interpretacion (Neg p) (x:xs) == True || interpretacion q (x:xs) == True
interpretacion (Syss p q) (x:xs) = interpretacion p (x:xs) == interpretacion q (x:xs)
