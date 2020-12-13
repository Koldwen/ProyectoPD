-- Proyecto PD.
-- Búsqueda en Escalada con reinicio
-- Enfriamiento Simulado
-- Algoritmo Genético
-- PSO
-- ARCHIVO: IA
    
import Prelude
import System.Random
import System.IO.Unsafe  -- be careful! 

--TIPOS
type Ind a = [a]
type Val = Double

type Gi a = (() -> Ind a)
type Gs a = (Ind a -> Ind a)
type Fv a = (Ind a -> Val)
type Me = (Val -> Val -> Bool)

type Solucion a = (Ind a, Val)
-- type Problema_enfriamiento = (gi, gs, fv, m, t, d, ne, ni)  

aleatorio :: (Random a, Num a) => a -> a -> a
aleatorio i j = unsafePerformIO (randomRIO (i,j))

prob :: (Random a, Enum a, Fractional a) => a
prob = [0.0001,0.0002..1]!!(unsafePerformIO (randomRIO (0,9999)))    

{--
e_simulado :: Gi a -> Gs a -> Fv a -> Me -> Double -> Double -> Int -> Int -> Solucion a
e_simulado gi gs fv me t d ne ni
    | ne <= 0 = error "El número de enfriamientos debe ser mayor que cero."
    | ni <= 0 = error "El número de iteraciones debe ser mayor que cero."
    | otherwise = e_simulado_enfr gs fv me t d ne ni actual' actual'
    where
        actual = gi()
        v_actual = fv actual
        actual' = (actual, v_actual)
--}

e_simulado_enfr :: (Show a) => Gs a -> Fv a -> Me -> Double -> Double -> Int -> Int -> Solucion a -> Solucion a -> IO()
e_simulado_enfr gs fv me t d ne ni actual mejor = do
    let t' = t * d
    let ne' = ne-1
    let nueva_iter = e_simulado_iter gs fv me t' d ni actual mejor
    if ne == 0
        then do
            putStrLn $ show mejor
            return()
        else do
            putStrLn $ show nueva_iter
            e_simulado_enfr gs fv me t' d ne' ni (fst nueva_iter) (snd nueva_iter)
        

e_simulado_iter :: Gs a -> Fv a -> Me -> Double -> Double -> Int -> Solucion a-> Solucion a -> (Solucion a, Solucion a)
e_simulado_iter _ _ _ _ _ 0 actual mejor = (actual, mejor) 
e_simulado_iter gs fv me t d ni actual mejor
    | aceptar_candidata = e_simulado_iter gs fv me t d l candidata n_mejor
    | otherwise = e_simulado_iter gs fv me t d l actual mejor
    where 
        sucesor = gs $ fst actual
        candidata = (sucesor, fv sucesor)
        incremento = snd candidata - snd actual
        --n_mejor = candidata
        n_mejor = if me (snd candidata) (snd mejor) then candidata else mejor
        l = ni - 1
        --aceptar_candidata = True
        aceptar_candidata = incremento < 0 || sorteo (snd candidata) (snd actual) t 


sorteo :: Double -> Double -> Double -> Bool             
sorteo vc va t
    | r <= p = True
    | otherwise = False
    where
        p = exp(-abs( (vc-va) / t)) :: Double
        r = prob :: Double

genera_inicial :: () -> [Int]
genera_inicial _ = [aleatorio 1 9 | _ <- [1..5]]

genera_sucesor :: [Int] -> [Int]
genera_sucesor actual = (take (i-1) actual) ++ [n] ++ (drop i actual)
    where
        i = aleatorio 1 (length actual-1)
        n = aleatorio 1 9

f_valoracion :: [Int] -> Double
f_valoracion l = fromIntegral $ sum l 

mejor :: Double -> Double -> Bool
mejor v_candidata v_actual = v_candidata > v_actual   

actual = genera_inicial()
sucesor = genera_sucesor actual

a = (actual, f_valoracion actual)
b = (sucesor, f_valoracion sucesor)