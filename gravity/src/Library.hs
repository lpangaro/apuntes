module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Requisito = (Persona -> Bool)
type Item = String

data Persona = UnaPersona {
    edad :: Number,
    items :: [Item],
    experiencia :: Number
} deriving Show

data Criatura = UnaCriatura {
    peligrosidad  :: Number,
    requisito :: Requisito
} deriving Show

siempredetras = UnaCriatura {peligrosidad = 0, requisito = const False}

tiene elemento persona = elem elemento (items persona)
gnomo cuantosGnomos = UnaCriatura { peligrosidad = 2 ^ cuantosGnomos, requisito = tiene "Soplador de hojas"}

fantasma categoria requisito = UnaCriatura {peligrosidad = 20 * categoria, requisito = requisito}
--fantasma 1 (\persona -> experiencia persona > 10)

cuantaExperiencia persona criatura  | requisito criatura persona = peligrosidad criatura
                                    | otherwise = 1

enfrentar persona criatura = persona {experiencia = experiencia perosna + cuantaExperiencia persona criatura}

--determinar cuanta experiencia es capaz de gana una persona luego de enfrentar sucesivamente a un grupo de personas
cuantaExperienciaAlEnfrentar persona criatura = experiencia (foldl enfrentar persona criaturas) - experiencia persona
--foldl


--SUCESIVAMENTE = FOLD

-- mostrar un ejemplo de consulta para el punto anterior...

requisitoFantasma persona = edad persona > 13 && tiene "disfraz de oveja" persona unaPersonaDeEjemplo = UnaPersona{edad 15, items = ["Soplador de hojas"], experiencia = 15}
cuantaExperienciaAlEnfrentar unaPersonaDeEjemplo [siempredetras, gnomo 10, fantasma 3 requisiroFantasma, fantasma 1 (>10).experiencia]












-- 2. Hacer que una persona se enfrente a una criatura, que implica que si esa persona puede deshacerse de
-- ella gane tanta experiencia como la peligrosidad de la criatura, o que se escape (que le suma en 1 la
-- experiencia, porque de lo visto se aprende) en caso de que no pueda deshacerse de ella


enfrentar persona criatura = requisito criatura persona









-- peligrosidadGnomo :: Number -> Number
-- peligrosidadGnomo cant = 2 ^ cant

-- peligrosidadFantasma :: Number -> Number
-- peligrosidadFantasma categoria = categoria * 20

-- deshacerseGnomo :: Pesona -> Bool
-- deshacerseGnomo persona = any (items persona)


-- enfrentarse :: Persona -> Criatura -> Bool
