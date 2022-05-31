module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Grito = (String, Number, Bool)
onomatopeya (o, _, _) = o
intensidad (_, i, _) = i
mojoLaCama (_, _, m) = m

type Niño = (String, Number, Number)
nombre (n, _, _) = n
edad (_, e, _) = e
altura (_, _, a) = a

--1
nivelDeTerror' grito = length (onomatopeya grito)
nivelDeTerror = length.onomatopeya

energiaDeGrito grito | mojoLaCama grito = nivelDeTerror grito * (intensidad grito ^ 2)
                     | otherwise = nivelDeTerror grito * 3 + intensidad grito

--2
sullivan niño = (grito, intensidad, mojoLaCama)
    where   grito = map (\ _ -> 'A') (nombre niño) ++ "GH"
            intensidad = 20 / (edad niño)
            mojoLaCama = edad niño < 3

randall niño = (grito, intensidad, mojoLaCama)
    where   grito = "!Mamadera!"
            intensidad = (length . filter esVocal . nombre) niño
            mojoLaCama = altura niño > 0.8 && altura niño < 1.2

esVocal letra = elem letra "AaEeIiOoUu"
esVocal' = flip elem "AaEeIiOoUu"
esVocal'' = (`elem` "AEIOUaeiou")

chuck niño = (['a'..'z'], 1000, True)

osito niño = ("uf", edad niño, False)

--3
pam :: [t -> b] -> t -> [b]
pam funciones valor = map (\funcion -> funcion valor) funciones

pam' valor [] = []
pam' (f:fs) valor = f valor : pam' fs valor

--4
gritos asustadores niño = pam asustadores niño

--5

obtenerListaDeGritos asustadores niños = concatMap (gritos asustadores) niños 
produccionEnergeticaGritos asustadores niños = sum (map energiaDeGrito (obtenerListaDeGritos asustadores niños))

--6
type Risa = (Number, Number)
duracion (d, _) = d
intensidadRisa (_, i) = i

energiaRisa risa = duracion risa ^ intensidadRisa risa

capusotto niño = ((edad niño) * 2, (edad niño) * 2 )

produccionEnergeticaRisas comediantes niños = (sumatoriaEnergiaRisas.hacerReirCampamento comediantes) niños
sumatoriaEnergiaRisas risas = (sum.map energiaRisa) risas
hacerReirCampamento comediantes niños = (concat.map (\niño -> pam niño comediantes)) niños