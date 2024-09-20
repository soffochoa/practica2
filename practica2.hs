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
