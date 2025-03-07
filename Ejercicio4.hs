-- Ejercicio 4

--Funcion que devulve True si es una tautologia (para todas las proposiciones el valor es true)
tautologia :: [(String, Bool)] -> Bool
tautologia proposiciones = all segundoElemento proposiciones

--Comparamos el segundo elemento de una tupla
segundoElemento :: (String, Bool) -> Bool
segundoElemento (x, y) = y == True

-- Metodo main
main :: IO ()
main = do
     let prueba1 =  [("p", False), ("q", True), ("r", False)]
     let prueba2 =  [("p", True), ("q", True), ("r", True)]     
     print (tautologia prueba1)
     print (tautologia prueba2)	


