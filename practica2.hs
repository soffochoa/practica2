--PRACTICA LÓGICA PROPOSICIONAL.

-- Definición del tipo LProp
data LProp = PTrue | PFalse | Var Nombre | Neg LProp | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp

type Nombre = String

--1. vars: Toma como argumento una fÓrmula y regresa las variables en la formula.

vars :: LProp -> [Nombre]
vars PTrue = []
vars PFalse = []
vars (Var n) = [n]
vars (Neg p) = vars p
vars (Conj p q) = vars p ++ vars q
vars (Disy p q) = vars p ++ vars q
vars (Impl p q) = vars p ++ vars q
vars (Syss p q) = vars p ++ vars q

--2. deMorgan: Toma una f´ormula proposicional y regresa su valor aplicando las leyes de De Morgan. Por ejemplo: deMorgan ¬(p ∧ q) regresa (¬p ∨ ¬q).
--Si nose aplica la ley, regresa la f´ormula original. 
deMorgan :: LProp -> LProp
deMorgan PTrue = PTrue
deMorgan PFalse = PFalse
deMorgan (Var n) = (Var n)
deMorgan (Neg n) = Neg (deMorgan n)
--deMorgan (Conj a b) = Conj (deMorgan a) (deMorgan b)
deMorgan (Neg (Conj a b)) = Disy (Neg (deMorgan a)) (Neg (deMorgan b))
--deMorgan (Disy a b) = Disy (deMorgan a) (deMorgan b)
deMorgan (Neg (Disy a b)) = Conj (Neg (deMorgan a)) (Neg (deMorgan b))
deMorgan (Impl a b) = Impl (deMorgan a) (deMorgan b)
deMorgan (Syss a b) = Syss (deMorgan a) (deMorgan b)

--3. equiv op: Dada una formula con implicacion → regresa su equivalente con conectores basicos. Por ejemplo, para p → q regresara el valor ¬p ∨ q.
--4.dobleNeg: Dada una formula con doble negacion elimina esta. Por ejemplo, para ¬(¬p) regresar p.
--5. num conectivos: Funcion recursiva para contar el n´umero de conectivos logicos de una formula
--6. num variables: Funcion recursiva para contar el n´umero de variables de una formula.
--7. profundidad: Funcion recursiva que regresa la profundidad de una f´ormula l´ogica.
--8.interpretacion: Regresa los valores de verdad, True o False, segun una asig-naci´on. Toma como argumentos una f´ormula y una asignaci´on de las variables.
--Por ejemplo: interpretacion (p∧q) [(”p”, True), (”q”, False)] regresar´a True.
