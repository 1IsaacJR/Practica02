-- Definición del tipo Prop
data Prop = Var String
          | Not Prop
          | And Prop Prop
          | Or  Prop Prop
          | Impl Prop Prop
          deriving (Show, Eq)

-- 1. Factores primos de un número natural
factoresPrimos :: Integer -> [Integer]
factoresPrimos n = factores n 2
  where
    factores 1 _ = []
    factores m d
      | m `mod` d == 0 = d : factores (m `div` d) d
      | otherwise      = factores m (d + 1)

-- 2. Codificación más eficiente (sin potencias enormes)
varCode :: String -> Integer
varCode s = sum (map (toInteger . fromEnum) s)

codificaProp :: Prop -> Integer
codificaProp (Var x)      = 1000 + varCode x
codificaProp (Not p)      = 2000 + codificaProp p
codificaProp (And p q)    = 3000 + codificaProp p * 2 + codificaProp q * 3
codificaProp (Or p q)     = 4000 + codificaProp p * 2 + codificaProp q * 3
codificaProp (Impl p q)   = 5000 + codificaProp p * 2 + codificaProp q * 3

-- 3. Verificar si un número corresponde a una fórmula dada
verificaCodigo :: Integer -> Prop -> Bool
verificaCodigo n f = n == codificaProp f

-- Programa principal con ejemplos
main :: IO ()
main = do
    putStrLn "== Ejemplo de Factores Primos =="
    let n = 84
    print ("Factores primos de " ++ show n ++ ": " ++ show (factoresPrimos n))

    putStrLn "\n== Ejemplo de Codificacion de Formulas =="
    let f1 = And (Var "p") (Not (Var "q"))
    let f2 = Or (Var "p") (Var "r")
    let cod1 = codificaProp f1
    let cod2 = codificaProp f2
    print ("Formula 1: " ++ show f1)
    print ("Codigo: " ++ show cod1)
    print ("Formula 2: " ++ show f2)
    print ("Codigo: " ++ show cod2)

    putStrLn "\n== Ejemplo de Verificacion =="
    print ("Codigo1 corresponde a f1?: " ++ show (verificaCodigo cod1 f1))
    print ("Codigo1 corresponde a f2?: " ++ show (verificaCodigo cod1 f2))
