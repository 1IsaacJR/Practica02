--Ejercicio1

-- Funcion arbol binario previamente definida
data ArbolBin = Vacio
-- Se realizaron cambios en el tipo de dato ArbolInt -> ArbolBin
      | Nodo ArbolBin Integer ArbolBin
      deriving(Show, Eq)

-- Funcion para contar la cantidad de nodos de un arbol binario
count :: ArbolBin  -> Integer
count Vacio  = 0
count (Nodo izq _ der) = 1 + count izq  + count der

--Funcion para contar la cantidad de niveles de profundidad
height :: ArbolBin  -> Integer
height Vacio  = 0
height (Nodo izq _ der) =  1 + max(height izq)(height der)
 
--Funcion dado un entero no negativo, devuelva un arbol binario balanceado
binarioBalanceado :: Integer -> ArbolBin
binarioBalanceado n
		  | n < 0	= Vacio
		  | otherwise = auxArbol 1 n

-- Función auxiliar que construye el árbol binario balanceado
auxArbol :: Integer -> Integer -> ArbolBin
auxArbol izq der
-- Caso base: cuando izquierda es mayor a derecha devolvemos Vacio
  | izq > der = Vacio  
  | otherwise =
-- Se toma el valor medio del numero como la raíz del árbol
      let medio = (izq + der) `div` 2
      	  nodo = Nodo (auxArbol izq (medio - 1)) medio (auxArbol (medio + 1) der)
      in nodo

-- Main de ejecucion
main :: IO ()
main = do
--Cambiar valores para binario balanceado
    let arbol1 = binarioBalanceado 10
    let arbol2 = Nodo (Nodo Vacio 2 Vacio) 1 (Nodo Vacio 3 Vacio)
    let cantidadNodos = count arbol2
    let alturaArbol = height arbol2

    print cantidadNodos
    print alturaArbol
    print arbol1
