--Ejercicio2

{-
Tipo de dato como guia de ayuda


data List a = Empty | Cons a (List a)

listInHaskell:: List a -> [a]
--caso base
listInHaskell Empty	= []
--Caso en el que convierte la lista
listInHaskell (Cons x xs) = x: listInHaskell xs


-}


--Funcion que corresponde a las variables de logica proposicional definida previamente

data Prop = Var String
 | Prop :& Prop
 | Prop :| Prop
 | Prop :/ Prop
 | No Prop
 deriving( Eq, Show )


--Funcion vars que identifica cuales son las variables proposicionales definida previamente

vars :: Prop -> [String]
vars (Var p)  = [p]
vars (p :& q) = vars p ++ vars q
vars (p :| q) = vars p ++ vars q
vars (p :/ q) = vars p ++ vars q
vars (No p)   = vars p


--Funcion para asignar valores recursivamente

asignaValoresRec :: [String] -> [[(String, Bool)]]
 -- Caso base, lista vacía
asignaValoresRec [] = [[]] 
--Para cada variable se le asigna un valor por defecto booleano
asignaValoresRec (x:xs) = [(x,True) : i | i <- asignaValoresRec xs]

--Funcion que asigna valores dependiendo de los valores predefinidos en la funcion asignaValoresRec

asignaValores:: [String] -> [[(String, Bool)]]
 -- Caso base, lista vacía
asignaValores [] = [[]]
 -- Compara que tipo de valores tiene variable proposicional
asignaValores vars = if (asignaValoresRec vars) ==  [[(head vars,True)]] then [[(head vars,True)]] else [[(head vars,False)]] 

{-


--asignaValores (vars p :& q) = asignaValores (p1) ++ asignaValores (p2) 
--asignaValores (No vars)   = if asignaValores (p1) == [[(p,True)]] then [[(p,False)]]


asignaValores (p1 :| p2) = asignaValores (p1) ++ asignaValores (p2)
asignaValores (p1 :/ p2) = asignaValores (p1) ++ asignaValores (p2)
asignaValores (No p1)   = asignaValores (p1) ++ asignaValores (p2)

-- Funcion que compara el segundo elemento del arreglo para saber su asignacion

compararSegundoElemento :: Eq a => [a] -> a -> Bool
compararSegundoElemento (x:(_:y:_)) valor = y == valor  -- Accedemos al segundo elemento (y) y lo comparamos
compararSegundoElemento _ _ = False  -- En caso de que la lista sea muy corta o esté vacía, retornamos False.


asignaValores (p :& q) = vars p ++ vars q
asignaValores (p :| q) = vars p ++ vars q
asignaValores (p :/ q) = vars p ++ vars q
asignaValores (No p)   = vars p

-}



