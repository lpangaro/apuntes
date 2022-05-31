module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Personaje = UnPersonaje {
    nombre :: String,
    planeta :: String,
    edad :: Number,
    energia :: Number,
    habilidades :: [String]
} deriving Show

data Guantelete = UnGuantelete {
    material :: String,
    cantGemas :: Number
} deriving Show

guantelete = UnGuantelete "uru" 6

ironMan = UnPersonaje "Tony" "Tierra" 38 80 ["Volar", "Disparar"]
drStrange = UnPersonaje "Steven" "Tierra" 40 100 ["Volar", "Escudo", "Sierra"]
groot = UnPersonaje "groot" "Marte" 27 50 ["Raices"]
wolverine = UnPersonaje "Logan" "Jupiter" 60 70 ["Garras", "Regeneracion"]
viudaNegra = UnPersonaje "Natasha" "Tierra" 30 50 ["Electricidad", "Mascara", "Cuerda"]

--type universo = [personaje]
universo = [ironMan, drStrange, groot, wolverine, viudaNegra]

sirve guantelete = (material guantelete == "uru") && (cantGemas guantelete == 6)
--                ((== "uru").material) guantelete

reducirPoblacion universo = take ((div) (length universo) 2) universo

chasquido guatelete universo | sirve guantelete = reducirPoblacion universo
                             |otherwise = universo

--2
menor personaje = (edad personaje) < 45
pendex universo = any menor universo 

energiaPersonaje personaje  | length (habilidades personaje) > 1 = energia personaje
                            | otherwise = 0

energiaTotal universo = sum (map energiaPersonaje universo)
energiaTotal' universo = (sum . map energiaPersonaje) universo
energiaTotal''  = sum . map energiaPersonaje 