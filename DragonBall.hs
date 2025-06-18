data Guerrero = Guerrero {
    ki           :: Float,
    fatiga       :: Float,
    personalidad :: Personalidad
}

type Ejercicio = Guerrero -> Guerrero

-- Punto 1 --

flexionesDeBrazos :: Ejercicio
flexionesDeBrazos = aumentarFatiga 50  

saltosAlCajon :: Float -> Ejercicio
saltosAlCajon altura = aumentarKi (altura / 5) . aumentarFatiga (altura / 10)

aumentarFatiga :: Float -> Guerrero -> Guerrero
-- aumentarFatiga fatigaAgregada guerrero = guerrero {fatiga = fatigaAgregada + fatiga guerrero}  
aumentarFatiga cantidadAumentar  = cambiarFatiga (+ cantidadAumentar)   

aumentarKi :: Float -> Guerrero -> Guerrero
-- aumentarKi kiAgregado guerrero = guerrero {ki = kiAgregado + ki guerrero} 
aumentarKi cantidadAumentar = cambiarKi (+ cantidadAumentar)


snatch :: Ejercicio
snatch guerrero 
    | esExperimentado guerrero = cambiarKi (* 1.05) . cambiarFatiga (* 1.10) $ guerrero
    | otherwise = aumentarFatiga 100 guerrero


cambiarKi :: (Float -> Float) -> Guerrero -> Guerrero
cambiarKi f guerrero = guerrero { ki = f (ki guerrero)}

cambiarFatiga :: (Float -> Float) -> Guerrero -> Guerrero
cambiarFatiga f guerrero = guerrero {fatiga = max 0 (f (fatiga guerrero))}

esExperimentado :: Guerrero -> Bool
esExperimentado guerrero = 22000 < ki guerrero

-- Punto 2 -- 

realizarEjercicio :: Ejercicio -> Guerrero -> Guerrero
realizarEjercicio ejercicio guerrero 
    | estaExhausto guerrero = cambiarKi (* 0.98) guerrero
    | estaCansado  guerrero = aumentarKi gains . aumentarFatiga cansancio $ guerrero
    | otherwise             = ejercicio guerrero
    where
        gains     = ki (ejercicio guerrero) - ki guerrero
        cansancio = fatiga (ejercicio guerrero) - fatiga guerrero

estaExhausto :: Guerrero -> Bool
estaExhausto = compararFatigaKi 72

estaCansado :: Guerrero -> Bool
estaCansado = compararFatigaKi 44

compararFatigaKi :: Float -> Guerrero -> Bool
compararFatigaKi porcentajeKi guerrero = fatiga guerrero < ki guerrero * (porcentajeKi / 100)

-- Punto 3 -- 

descansar :: Float -> Guerrero -> Guerrero 
descansar minutos = disminuirFatiga . sum $ [0..minutos]

disminuirFatiga :: Float -> Guerrero -> Guerrero
disminuirFatiga cansancio = cambiarFatiga (subtract cansancio)

descansoOpitmo :: Guerrero -> Float
descansoOpitmo guerrero = head . dropWhile (estaCansado . flip descansar guerrero) $ [0..] 

-- Punto 4 --

data Personalidad = Sacado | Perezoso | Tramposo

type Rutina = [Ejercicio]

realizarRutina :: Rutina -> Guerrero -> Guerrero 
realizarRutina rutina guerrero = foldr realizarEjercicio guerrero rutina 

rutinaSegunPersonalidad :: Personalidad -> Rutina -> Rutina 
rutinaSegunPersonalidad Sacado rutina = rutina
rutinaSegunPersonalidad Perezoso rutina = concatMap (\ejercicio -> [ejercicio, descansar 5]) rutina 
rutinaSegunPersonalidad Tramposo rutina = []