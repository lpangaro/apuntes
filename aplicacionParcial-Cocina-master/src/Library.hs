module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

triple :: Number -> Number
triple numero = numero + numero + numero

between:: Ord a => a -> a -> a -> Bool
between minimo maximo elemento = minimo <= elemento && maximo >= elemento

f7 = even.length

f8 = map doble.filter even

---------



------------

data Alimento = UnAlimento {
    nombre:: String,
    caracteristicas:: [String],
    sabor:: Number 
} deriving Show

choclo = UnAlimento "choclo" ["lindo","amarillo"] 50
cala = UnAlimento "calabaza" ["maduro"] 60
calapodrida = UnAlimento "calabaza" ["maduro","podrido"] 60

carne = UnAlimento "bife" [] 120

algunosAlimentos = [choclo,cala,carne]


plancha:: Alimento->Alimento
plancha (UnAlimento nombre caracteristicas sabor) = 
  (UnAlimento nombre ("cocido":caracteristicas) (sabor + 10)) 

provoletera:: Alimento->Alimento
provoletera (UnAlimento nombre caracteristicas sabor) = 
  (UnAlimento nombre caracteristicas (doble sabor)) 



-- Es buena cocina si cocino todos untos a la plancha una serie de alimentos 
-- y consiguo que tenga buen sabor

buenPlatoALaPlancha:: [Alimento] -> Bool
buenPlatoALaPlancha alimentos = buenPlato  plancha  alimentos
  
buenPlatoALaProvoletera:: [Alimento] -> Bool
buenPlatoALaProvoletera alimentos = buenPlato  provoletera  alimentos
 
buenPlato:: (Alimento -> Alimento) -> [Alimento] -> Bool
buenPlato  utensillo  alimentos = sum (map sabor (map utensillo alimentos)) > 200

---------------------

olla :: String -> Alimento -> Alimento
olla liquido alimento = 
  UnAlimento (nombre alimento) ("cocido":caracteristicas alimento) 
  (sabor alimento * incremento liquido)

incremento "aceite" = 1.8
incremento "agua" = 1.2
incremento x = 1


cocinaCon::(Alimento -> Alimento)->Alimento -> Number
cocinaCon accesorio alimento 
  | enMalEstado alimento = 0
  | otherwise = sabor (accesorio alimento)



enMalEstado alimento = elem "podrido" (caracteristicas alimento)

academiaFritanga cocinero = cocinero{accesorioPreferido = olla "aceite"}
academiaSuperFritanga cocinero = cocinero{accesorioPreferido = olla "aceite".accesorioPreferido cocinero}
academia accesorioAdicional cocinero = cocinero{accesorioPreferido = accesorioAdicional.accesorioPreferido cocinero}
--academiaMultiple accesorios cocinero = cocinero{accesorioPreferido = last accesorios.accesorioPreferido cocinero}

---------------------------

--Los cocineros
data Cocinero = UnCocinero {
  apodo :: String,
  accesorioPreferido:: Alimento -> Alimento
}

german = UnCocinero "German" plancha
donato = UnCocinero "Donato" (olla "aceite")
damian = UnCocinero "Damian" (olla "agua".provoletera)
aguanteLaFritanga = UnCocinero "el rey del aciete" (olla "aceite".olla "aceite".olla "aceite")

cocinaBien:: Cocinero -> [Alimento] -> Bool
cocinaBien cocinero alimentos = buenPlato (accesorioPreferido cocinero) alimentos


-----------------------

--freir tomate = olla "aceite" tomate
freir = olla "aceite"
hervir = olla "agua"

pii = 3.14
-----------------





laMejorFuncion::( Ord b) => (a-> b) -> (a-> b) -> a -> (a -> b)

laMejorFuncion funcion1 funcion2 valor 
 | funcion1 valor > funcion2 valor = funcion1 
 | otherwise = funcion2 




f2 = even

losTresPrimeros = take 3

f4 = (2/)

f5 = (/2)


duplicaYSuma3YSeFijaSiEsPar x = even (x * 2 + 3)

aplicacionSucesiva x y z = x (y z)
