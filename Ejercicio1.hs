--Ejercicio1

-- Funcion arbol binario previamente definida
data ArbolBin = Vacio
-- Se realizaron cambios en el tipo de dato ArbolInt -> ArbolBin
     	      | Nodo ArbolBin Integer ArbolBin
	      deriving(Show, Eq)

-- Funcion para contar la cantidad de nodos de un arbol binario
count :: ArbolBin  -> Integer
count Vacio  = 0
count (Nodo izq _ der)
  | izq == Vacio && der == Vacio  = 0 + count izq + count der 
  | otherwise = 1 + count izq  + count der 

--Funcion para contar la cantidad de niveles de profundidad
height :: ArbolBin  -> Integer
height Vacio  = 0
height (Nodo izq _ der) 
  | izq == Vacio && der == Vacio  = 1 + count izq + count der 
  | otherwise = count izq  + count der 

--Funcion dado un entero no negativo, devuelva un arbol binario balanceado
binarioBalanceado :: Integer -> ArbolBin
binarioBalanceado n = construyeArbol 1 n

-- Función auxiliar que construye el árbol binario balanceado
construyeArbol :: Integer -> Integer -> ArbolBin
construyeArbol inicio fin
-- Caso base: cuando el rango es inválido, devolvemos Vacio
  | inicio > fin = Vacio  
  | otherwise =
-- Se toma el valor medio del numero como la raíz del árbol
      let medio = (inicio + fin) `div` 2
          nodo = Nodo (construyeArbol inicio (medio - 1)) medio (construyeArbol (medio + 1) fin) in nodo

