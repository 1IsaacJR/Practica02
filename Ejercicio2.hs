--Ejercicio2

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

asignaValoresRec :: [String] -> [(String, Bool)]
 -- Caso base, lista vacía
asignaValoresRec [] = [] 
--Para cada variable se le asigna un valor por defecto booleano
asignaValoresRec (x:xs) = (x,True) : asignaValoresRec xs

--Funcion que asigna valores dependiendo de los valores predefinidos en la funcion asignaValoresRec

asignaValores:: [String] -> [(String, Bool)]
 -- Caso base, lista vacía
asignaValores [] = []
 -- Compara que tipo de valores tiene variable proposicional
asignaValores vars
	      | (asignaValoresRec vars) ==  [(head vars,True)]	=  [(head vars,True)] --else [(head vars,False)]
	      |	otherwise = (head vars, False) : asignaValores (tail vars)
 
-- Redefinir la función tail tal que tail de "1" es el resto de la lista 1.
n_tail :: [a] -> [a]
n_tail (_:xs) = xs


-- Ejemplo de una fórmula lógica
ejemplo :: Prop
ejemplo = (Var "p" :& Var "q") :| No (Var "r")

-- Uso de las funciones

-- Obtención de las variables de la fórmula
variables = vars ejemplo
-- Asignación de valores a las variables
valores = asignaValores variables

main :: IO ()
main = do
    print "Variables proposicionales:"
    print variables
    print "Asignaciones de valores:"
    print valores



