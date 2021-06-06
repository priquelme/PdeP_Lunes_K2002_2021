
-- TP Monopoly

-- Pablo Daniel Riquelme Blaffet

-- PdeP Lunes 2021


import Text.Show.Functions ()

data Participante = Participante {
    nombre :: Nombre,
    cantidadDeDinero :: CantidadDeDinero,
    tactica :: Tactica, 
    propiedades :: [Propiedad],
    acciones :: [Accion] 
} deriving Show

data Propiedad = Propiedad {
    nombrePropiedad :: NombrePropiedad,
    precio :: PrecioPropiedad
} deriving Show

type NombrePropiedad = String
type PrecioPropiedad = Int
type Nombre = String
type CantidadDeDinero = Int
type Tactica = String
type Accion = Participante -> Participante


-- Participantes

manuel :: Participante
manuel = Participante "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]

carolina :: Participante
carolina = Participante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionista] 

-- Acciones


pasarPorElBanco :: Accion
pasarPorElBanco = mapTactica (const "Comprador Compulsivo") . mapCantidadDinero (+ 40) 


enojarse :: Accion
enojarse = mapAccion ((:) gritar) . mapCantidadDinero (+ 50)


gritar :: Accion
gritar = mapNombre ("AHHHH" ++)


subastar :: Propiedad -> Accion
subastar unaPropiedad unParticipante 
    | tieneTactica . tactica $ unParticipante = comprarCasa unaPropiedad unParticipante
    | otherwise                               = unParticipante


cobrarAlquileres :: Accion
cobrarAlquileres  unParticipante = mapCantidadDinero (+ totalAlquiler unParticipante) unParticipante


pagarAAccionista :: Accion
pagarAAccionista unParticipante
    | tieneTactica . tactica $ unParticipante  = mapCantidadDinero (+ 200) unParticipante
    | otherwise                                = mapCantidadDinero (subtract 100) unParticipante


hacerBerrinche :: Propiedad -> Accion
hacerBerrinche unaPropiedad unParticipante
    | precio unaPropiedad > cantidadDeDinero unParticipante = hacerBerrinche unaPropiedad (mapAccion ((:) gritar) . mapCantidadDinero (+ 10) $ unParticipante) -- ¿Repetición de Lógica?
    | otherwise                                             = comprarCasa unaPropiedad unParticipante


ultimaRonda :: Participante -> Accion
ultimaRonda unParticipante = foldl1 (.) $ acciones unParticipante

juegoFinal :: Participante -> Participante -> Accion
juegoFinal unParticipante otroParticipante
    | (< dineroFinalTotal unParticipante) . dineroFinalTotal $ otroParticipante = ultimaRonda unParticipante
    | otherwise                                                                 = ultimaRonda otroParticipante


-- Funciones Auxiliares

comprarCasa :: Propiedad -> Accion
comprarCasa unaPropiedad = mapPropiedad ((:) unaPropiedad) . mapCantidadDinero (subtract . precio $ unaPropiedad) 

tieneTactica :: Tactica -> Bool
tieneTactica "Accionista"        = True
tieneTactica "Oferente Singular" = True
tieneTactica _                   = False

preciosPropiedades :: Participante -> [Int]
preciosPropiedades = (map precio) . propiedades

cantidadDePropiedadesCaras :: Participante -> Int
cantidadDePropiedadesCaras unParticipante = length $ filter (< 150) (preciosPropiedades unParticipante)

alquilerDePropiedadesCaras :: Participante -> Int
alquilerDePropiedadesCaras = (* 20) . cantidadDePropiedadesCaras

propiedadesTotales :: Participante -> Int
propiedadesTotales = length . propiedades

totalAlquiler :: Participante -> Int
totalAlquiler unParticipante = (+) (alquilerDePropiedadesCaras unParticipante) (10 * (propiedadesTotales unParticipante - cantidadDePropiedadesCaras unParticipante))

dineroFinalTotal :: Participante -> Int
dineroFinalTotal unParticipante = cantidadDeDinero . (ultimaRonda unParticipante) $ unParticipante

mapNombre :: (Nombre -> Nombre) -> Participante -> Participante
mapNombre funcion unParticipante = unParticipante {nombre = funcion . nombre $ unParticipante}

mapCantidadDinero :: (CantidadDeDinero -> CantidadDeDinero) -> Participante -> Participante
mapCantidadDinero funcion unParticipante = unParticipante {cantidadDeDinero = funcion . cantidadDeDinero $ unParticipante}

mapTactica :: (Tactica -> Tactica) -> Participante -> Participante
mapTactica funcion unParticipante = unParticipante {tactica = funcion . tactica $ unParticipante}

mapPropiedad :: ([Propiedad] -> [Propiedad]) -> Participante -> Participante
mapPropiedad funcion unParticipante = unParticipante {propiedades = funcion . propiedades $ unParticipante}

mapAccion :: ([Accion] -> [Accion]) -> Participante -> Participante
mapAccion funcion unParticipante = unParticipante {acciones = funcion . acciones $ unParticipante}

-- Arreglar declaratividad cobrarAlquileres

{-
esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata propiedad = precioPropiedad propiedad < 150

precioAlquiler :: Propiedad -> Int
precioAlquiler propiedad
  | esPropiedadBarata propiedad = 10
  | otherwise                   = 20

ingresosPorAlquileres :: Persona -> Int
ingresosPorAlquileres persona = sum . map precioAlquiler . propiedades $ persona

cobrarAlquileres :: Accion
cobrarAlquileres persona = cambiarDinero ((+).ingresosPorAlquileres $ persona) persona
-}