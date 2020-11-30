-- Proyecto PD.
-- Búsqueda en Escalada con reinicio
-- Enfriamiento Simulado
-- Algoritmo Genético
-- PSO
-- ARCHIVO: IA
module IA where

import System.Random
import System.IO.Unsafe  -- be careful! 

aleatorio :: (Random a, Num a) => a -> a -> a
aleatorio i j = unsafePerformIO (randomRIO (i,j))

prob :: (Random a, Enum a, Fractional a) => a
prob = [0.0001,0.0002..1]!!(unsafePerformIO (randomRIO (0,9999)))    

b_escalada :: (Num b) => ([a]) -> ([a] -> [a]) -> ([a] -> b) -> (b -> b -> Bool) -> ([a], b)
b_escalada genera_inicial genera_sucesor f_valoracion mejor = 
    b_escalada_aux actual sucesor genera_sucesor f_valoracion mejor
    where
        sucesor = genera_sucesor actual
        actual = genera_inicial

b_escalada_aux :: (Num b) => [a] -> [a] -> ([a] -> [a]) -> ([a] -> b) -> (b -> b -> Bool) -> ([a], b)
b_escalada_aux actual vecino genera_sucesor f_valoracion mejor
    | mejor v_vecino v_actual = b_escalada_aux vecino sucesor genera_sucesor f_valoracion mejor
    | otherwise = (actual, v_actual)
    where
        sucesor = genera_sucesor vecino
        v_actual = f_valoracion actual
        v_vecino = f_valoracion vecino

b_escalada_reinicio :: (Num b) => ([a]) -> ([a] -> [a]) -> ([a] -> b) -> (b -> b -> Bool) -> Int -> ([a], b)
b_escalada_reinicio genera_inicial genera_sucesor f_valoracion mejor n_reinicios =
    b_escalada_reinicio_aux genera_inicial genera_sucesor f_valoracion mejor (n_reinicios-1) sol
    where
        sol = b_escalada genera_inicial genera_sucesor f_valoracion mejor

b_escalada_reinicio_aux :: (Num b) => ([a]) -> ([a] -> [a]) -> ([a] -> b) -> (b -> b -> Bool) -> Int -> ([a], b) -> ([a], b)
b_escalada_reinicio_aux genera_inicial genera_sucesor f_valoracion mejor n_reinicios sol
    | n_reinicios == 0   = sol
    | mejor (snd p) (snd sol) = b_escalada_reinicio_aux genera_inicial genera_sucesor f_valoracion mejor (n_reinicios-1) p
    | otherwise = b_escalada_reinicio_aux genera_inicial genera_sucesor f_valoracion mejor (n_reinicios-1) sol
    where
        p = b_escalada genera_inicial genera_sucesor f_valoracion mejor