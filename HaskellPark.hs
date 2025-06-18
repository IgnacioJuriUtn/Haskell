data Atraccion = Atraccion {
    nombre             :: String,
    alturaMinima       :: Int,
    duracion           :: Int,
    opiniones          :: [String],
    mantenimiento      :: Bool,
    reparaciones       :: [(Reparacion, Float)]
}

-- Punto 1 --

puntuarAtraccion :: Atraccion -> Int
puntuarAtraccion atraccion 
    | duracionProlongada atraccion = 100
    | pocasReparaciones atraccion  = length (nombre atraccion) *10 + length (opiniones atraccion) *2
    | otherwise = alturaMinima atraccion * 10

duracionProlongada :: Atraccion -> Bool
duracionProlongada atraccion = duracion atraccion > 10

pocasReparaciones :: Atraccion -> Bool
pocasReparaciones atraccion = length (reparaciones atraccion) < 3

-- Punto 2 -- 

type Reparacion = Atraccion -> Atraccion

ajusteDeTornilleria :: Int -> Reparacion
ajusteDeTornilleria tornillos atraccion = atraccion {
    duracion = min 10 (tornillos + duracion atraccion)
}

engrase :: Float -> Reparacion
engrase grasa atraccion = atraccion {
    alturaMinima = (grasa * 0.1) + alturaMinima atraccion,
    opiniones = "para valientes" : opiniones atraccion 
}

mantenimientoElectrico :: Reparacion
mantenimientoElectrico atraccion = atraccion {
    opiniones = take 2 . opiniones $ atraccion
} 

mantenimientoBasico :: Reparacion
mantenimientoBasico  = ajusteDeTornilleria 8 . engrase 10 

-- Punto 3 -- 
 
esaMeDaMiedito :: Atraccion -> Bool
esaMeDaMiedito = any (\(_,tiempo) -> tiempo > 4) . reparaciones
-- Devuelve True si algún elemento de la lista cumple la condición.

acaCerramos :: Atraccion -> Bool
acaCerramos atraccion = tiempoDeReparacionAtraccion atraccion == 7

tiempoDeReparacionAtraccion :: Atraccion -> Float 
tiempoDeReparacionAtraccion = sum . map snd . reparaciones 

data Parque = Parque {
    atracciones :: [Atraccion]
}

disneyNoEsistis :: Parque -> Bool
disneyNoEsistis  =   all (null . reparaciones) . filter (\a -> length (nombre a) > 5) . atracciones 
  
-- Punto 4 -- 

reparacionesPiolas :: Atraccion -> Bool
reparacionesPiolas atraccion = todasMejoran atraccion (map fst (reparaciones atraccion))

esMasPiola :: Reparacion -> Atraccion -> Bool
esMasPiola reparacion atraccion = puntuarAtraccion (reparacion atraccion) > puntuarAtraccion atraccion

todasMejoran :: Atraccion -> [Reparacion] -> Bool
todasMejoran atraccion [] = True
todasMejoran atraccion (r:rs) 
    | esMasPiola r atraccion = todasMejoran atraccionReparada rs
    | otherwise = False
    where 
        atraccionReparada = r atraccion


-- Punto 5 -- 

repararAtraccion :: Atraccion -> Atraccion
repararAtraccion atraccion = foldl (\acc reparacion -> reparacion acc) atraccion (map fst (reparaciones atraccion))
