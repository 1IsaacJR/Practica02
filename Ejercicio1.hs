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


{-
height:: ArbolBin -> Integer
height (Nodo izq _ der)
       | izq ==  Vacio && der == Vacio	=	0
       | otherwise = 1 + height izq + height der 
--contador
cou:: Integer -> Integer
cou n = n + 1

-}


data TGByPrice = TGByPrice TravelGuide
instance Ord TravelGuide where
	 (TravelGuide t1 a1 p1) <= (TravelGuide t2 a2 p2) =
	 p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

--Funcion dado un entero no negativo, devuelva un arbol binario balanceado
binarioBalanceado:: ArbolBin-> Integer ->ArbolBin
binarioBalanceado Vacio n = if n < 0 then Vacio 
binarioBalanceado (Nodo izq _ der n )
		  | binarioBalanceado izq _ n /= Vacio	= binarioBalanceado izq n-1
		  | otherwise = binarioBalanceado der n-1

{-

a < b para todo binarioBalanceado (izq)
b > a para todo binarioBalanceado (der)

data Comparacion = Vacio | a < b
	      deriving(Show, Eq)
	      
BinarioBalanceado (Nodo izq der aux) n
		  | n <= 0 =  Vacio
		  | n > 0  =  ArbolBinario
--		  | izq <=  der	=	 = der = izq 
		  | otherwise ArbolBinario izq 

-}

