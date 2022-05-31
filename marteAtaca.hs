data Enfermedad = UnaEnfermedad Float String deriving Show
tasaDeMortalidad (UnaEnfermedad tasaDeMortalidad _) = tasaDeMortalidad
medidaDeCombate (UnaEnfermedad _ medidaDeCombate) = medidaDeCombate

data Planeta = Planeta Int [String] [String] deriving Show
poblacionTotal (Planeta poblacionTotal _ _) = poblacionTotal
continentes (Planeta _ continentes _) = continentes
medidasTomadas (Planeta _ _ medidasTomadas) = medidasTomadas


-- Ejemplo de Enfermedades

coronaVirus :: Enfermedad
coronaVirus = UnaEnfermedad 3.5 "cuarentena"

dengue :: Enfermedad
dengue = UnaEnfermedad 2.7 "Evitar la acumulacion de agua"

peste :: Enfermedad
peste = UnaEnfermedad 1 "lavarse las manos"

-- Ejemplos de Planetas

tierraAntesDeCuarentena :: Planeta
tierraAntesDeCuarentena = Planeta 7777382699 ["America", "Asia", "Africa", "Europa", "Oceania", "Antartida"] ["lavarse las manos"]

marte :: Planeta
marte = Planeta 2 [] ["cuarentena","lavarse las manos","Evitar la acumulacion de agua"]

venus :: Planeta
venus = Planeta 15 ["Comuna 1","Comuna 2","Comuna 3","Comuna 4","Comuna 5"] ["cuarentena","evitar lugares cerrados","usar barbijo","lavarse las manos"]

-- Punto 1: 

combinarEnfermedades :: Enfermedad -> Enfermedad -> Enfermedad
combinarEnfermedades enfermedad1 enfermedad2 = UnaEnfermedad (tasaDeMortalidad enfermedad1 + tasaDeMortalidad enfermedad2) (medidaDeCombate enfermedad1)

-- Punto 2

tomarMedidasDeProteccion :: Planeta -> Enfermedad -> Planeta
tomarMedidasDeProteccion planeta enfermedad 
    | planetaProtegidoContra planeta enfermedad = planeta 
    | otherwise = implementarMedida planeta (medidaDeCombate enfermedad)

planetaProtegidoContra :: Planeta -> Enfermedad -> Bool                                            
planetaProtegidoContra planeta enfermedad = elem (medidaDeCombate enfermedad) (medidasTomadas planeta)

implementarMedida :: Planeta -> String -> Planeta
implementarMedida (Planeta poblacionTotal continentes medidasTomadas) medida = Planeta poblacionTotal continentes (medida:medidasTomadas)
--implementarMedida planeta medida = planeta{medidasTomadas = medidasTomadas planeta ++ [medida]}
--implementarMedida (Planeta p c medidas) medida = Planeta p c (medida:medidas)
--implementarMedida (Planeta p c medidas) medida = Planeta p c (medidas++[medida])


-- Punto 3

estaAlHorno :: Planeta -> Enfermedad -> Bool
estaAlHorno planeta enfermedad = not (planetaProtegidoContra planeta enfermedad) && cantidadDeVictimas planeta enfermedad > 1000000

cantidadDeVictimas :: Planeta -> Enfermedad -> Int
cantidadDeVictimas planeta enfermedad =   ceiling (tasaDeMortalidad enfermedad) * poblacionTotal planeta `div` 100
--cantidadDeVictimas planeta enfermedad =   ceiling (tasaDeMortalidad enfermedad * fromIntegral (poblacionTotal planeta) / 100)

-- Punto 4
tieneMasMedidasQueHabitantesPorContinente :: Planeta -> Bool
tieneMasMedidasQueHabitantesPorContinente planeta = cantidadMedidasPrevencion planeta > cantidadHabitantesPorContinente planeta

cantidadMedidasPrevencion :: Planeta -> Int
cantidadMedidasPrevencion planeta = length (medidasTomadas planeta)

cantidadHabitantesPorContinente :: Planeta -> Int
cantidadHabitantesPorContinente (Planeta poblacion [] _ ) = poblacion 
cantidadHabitantesPorContinente (Planeta poblacion conts _ ) = poblacion `div` (length conts)

-- Punto 5
enfermedadAtacaPlaneta :: Enfermedad -> Planeta -> (Planeta,Int)
enfermedadAtacaPlaneta enfermedad planeta 
    | planetaProtegidoContra planeta enfermedad =  (planeta, 0)
    | otherwise = ( planetaDespuesDelAtaque planeta enfermedad , cantidadDeVictimas planeta enfermedad )

planetaDespuesDelAtaque :: Planeta -> Enfermedad -> Planeta
planetaDespuesDelAtaque planeta enfermedad = tomarMedidasDeProteccion ( atacarPlaneta planeta enfermedad ) enfermedad

atacarPlaneta :: Planeta -> Enfermedad -> Planeta
atacarPlaneta planeta enfermedad =
  Planeta ((poblacionTotal planeta) - (cantidadDeVictimas planeta enfermedad)) (continentes planeta) (medidasTomadas planeta)


-- Punto 6
 
tieneMasVictimasLaPrimeraCombinadaConLaSegunda :: Enfermedad -> Enfermedad -> Planeta -> Bool
tieneMasVictimasLaPrimeraCombinadaConLaSegunda enfermedad1 enfermedad2 planeta = 
    victimasAtaqueCombinado enfermedad1 enfermedad2 planeta > victimasAtaqueCombinado enfermedad2 enfermedad1 planeta

victimasAtaqueCombinado :: Enfermedad -> Enfermedad -> Planeta -> Int
victimasAtaqueCombinado enfermedad1 enfermedad2 planeta = snd ( enfermedadAtacaPlaneta (combinarEnfermedades enfermedad1 enfermedad2) planeta )
