--Ejercicio3
--Importacion del tipo de dato Just
import Data.Maybe (fromJust)

-- Funcion de proposiciones logicas
data Prop = Var String
          | Prop :& Prop
          | Prop :| Prop
          | Prop :/ Prop
          | No Prop
          deriving (Eq, Show)

-- Función de interpretación
interpretacion :: Prop -> [(String, Bool)] -> Bool
interpretacion (Var p) i = fromJust (lookup p i)
interpretacion (p :& q) i = interpretacion p i && interpretacion q i
interpretacion (p :| q) i = interpretacion p i || interpretacion q i
interpretacion (p :/ q) i = not (interpretacion p i) || interpretacion q i
interpretacion (No p) i = not (interpretacion p i)

-- Ejemplo de fórmula lógica
ejemplo :: Prop
ejemplo = (Var "p" :& Var "q") :| No (Var "r")

-- Ejemplo de interpretación
interpretacionEjemplo :: [(String, Bool)]
interpretacionEjemplo = [("p", False), ("q", True), ("r", False)]

-- Metodo main
main :: IO ()
main = do
    print $ interpretacion ejemplo interpretacionEjemplo