module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


cauntosCaramelosQuedan cantidad [] = cantidad -- un caso base
cauntosCaramelosQuedan cantidadDeCaramelos (persona : personas) 
    | cantidadDeCaramelos > 0 = cauntosCaramelosQuedan (cantidadDeCaramelos - cauntosCaramelos persona cantidadDeCaramelos) personas
    | otherwise = 0 --tambien es un caso base. es un numero, no llama de nuevo a la funcion

cantidadDeCaramelos (tiene, quiere)
cuantosQuedan = min cuantosQuedan (quiere - tiene)

cauntosCaramelosQuedan' cantidad [] = cantidad
cauntosCaramelosQuedan' cantidadDeCaramelos (persona : personas) =  cauntosCaramelosQuedan' (sacarCaramelos cantidadDeCaramelos persona) personas

sacarCaramelos cuantosQuedan persona = max (cuantosQuedan - cauntosCaramelos persona cuantosQuedan) 0

--cauntosCaramelosQuedan' -- lo reemplazo por foldear
foldear valor [] = valor
foldear cantidadDeCaramelos (x:xs) = foldear (sacarCaramelos cantidadDeCaramelos x) xs

-- sacarCaramelos -- es una funcion. lo remmplazo por funcion
-- cantidadDeCaramelos -- es un acumulador
foldear' _ acumumlador [] = acumumlador
foldear' funcion acumulador (x:xs) = foldear' funcion (funcion acumulador x) xs


cauntosCaramelosQuedan'' caramelos personas = foldl sacarCaramelos caramelos personas

min y max los puedo usar para reemplazar una tupla

foldl -> de izquierda a derecha
foldr -> de derecha a izquierda

foldl (/) 1 [1,2,3]
1/1 = 1
1/2 = 0.5
0.5 / 3 = 0.166666

foldr (/) 1 [1,2,3]
3/1 = 3
3/2 = 1.5
1.5/1 = 1.5

foldl1 (+) [1,2,4] -> el acumulador es el primer elemento de la lista

