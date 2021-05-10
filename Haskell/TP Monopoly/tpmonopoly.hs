
-- TP Monopoly

-- Pablo Daniel Riquelme Blaffet

-- PdeP Lunes 2021


import Text.Show.Functions ()

data Participante = Participante {
    nombre :: Nombre,
    cantidadDeDinero :: CantidadDeDinero,
    tactica :: [Tactica], 
    propiedad :: [Propiedad],
    accion :: [Accion] 
} deriving Show

type Nombre = String
type CantidadDeDinero = Int
type Tactica = String
type Propiedad = (String, Int)
type Accion = Participante -> Participante


-- Participantes

manuel :: Participante
manuel = Participante "Manuel" 500 ["Oferente singular"] [] [pasarPorElBanco, enojarse]

carolina :: Participante
carolina = Participante "Carolina" 500 ["Accionista"] [] [pasarPorElBanco, pagarAAccionista] 

-- CASO PARA PRUEBAS --

javier :: Participante 
javier = Participante "Javier R." 700 ["Prestamista", "Accionista"] [("Casita Feliz", 300), ("Casita Hiper Feliz", 145), ("Casa en la playa", 150), ("Casa en la montania", 120)] [pasarPorElBanco, gritar, enojarse]

-- CASO PARA PRUEBAS --


-- Acciones


pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = unParticipante {cantidadDeDinero = (+ 40) . cantidadDeDinero $ unParticipante, tactica = ["Comprador compulsivo"]} 


enojarse :: Accion
enojarse unParticipante = unParticipante {cantidadDeDinero = (+ 50). cantidadDeDinero $ unParticipante, accion = (++ [gritar]) . accion $ unParticipante} 


gritar :: Accion
gritar unParticipante = unParticipante {nombre = ("AHHHH " ++) . nombre $ unParticipante}


subastar :: Propiedad -> Accion
subastar unaPropiedad unParticipante 
    | tieneTactica "Oferente Singular" unParticipante || tieneTactica "Accionista" unParticipante = comprarCasa unaPropiedad unParticipante
    | otherwise                                                                                   = unParticipante


cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = unParticipante {cantidadDeDinero = (+) (cantidadDeDinero unParticipante) (totalAlquiler unParticipante) }


pagarAAccionista :: Accion
pagarAAccionista unParticipante
    | tieneTactica "Accionista" unParticipante = unParticipante {cantidadDeDinero = (+ 200) . cantidadDeDinero $ unParticipante} 
    | otherwise                                = unParticipante {cantidadDeDinero = (+ (-100)) . cantidadDeDinero $ unParticipante}
                                                                                                       -- No me deja poner (-100)

hacerBerrinche :: Propiedad -> Accion
hacerBerrinche unaPropiedad unParticipante
    | snd unaPropiedad > cantidadDeDinero unParticipante = hacerBerrinche unaPropiedad unParticipante {cantidadDeDinero = (+ 10) . cantidadDeDinero $ unParticipante, accion = accion unParticipante ++ [gritar]}
    | otherwise                                          = comprarCasa unaPropiedad unParticipante


ultimaRonda :: Participante -> Accion
ultimaRonda unParticipante = foldl1 (.) $ accion unParticipante

juegoFinal :: Participante -> Participante -> Participante
juegoFinal unParticipante otroParticipante
    | (< dineroFinalTotal unParticipante) . dineroFinalTotal $ otroParticipante = unParticipante
    | otherwise                                                                 = otroParticipante


-- Funciones Auxiliares

comprarCasa :: Propiedad -> Accion
comprarCasa unaPropiedad unParticipante = unParticipante {cantidadDeDinero = (cantidadDeDinero unParticipante -) . snd $ unaPropiedad, propiedad = (++ [unaPropiedad]) . propiedad $ unParticipante}

tieneTactica :: Tactica -> Participante -> Bool
tieneTactica unaTactica unParticipante = elem unaTactica (tactica unParticipante)

preciosPropiedades :: Participante -> [Int]
preciosPropiedades = (map snd) . propiedad

cantidadDePropiedadesCaras :: Participante -> Int
cantidadDePropiedadesCaras unParticipante = length $ filter (< 150) (preciosPropiedades unParticipante)

alquilerDePropiedadesCaras :: Participante -> Int
alquilerDePropiedadesCaras = (* 20) . cantidadDePropiedadesCaras

propiedadesTotales :: Participante -> Int
propiedadesTotales = length . propiedad 

totalAlquiler :: Participante -> Int
totalAlquiler unParticipante = (+) (alquilerDePropiedadesCaras unParticipante) (10 * (propiedadesTotales unParticipante - cantidadDePropiedadesCaras unParticipante))

dineroFinalTotal :: Participante -> Int
dineroFinalTotal unParticipante = cantidadDeDinero . (ultimaRonda unParticipante) $ unParticipante
