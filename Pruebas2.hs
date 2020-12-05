import System.Random
import System.IO.Unsafe  -- be careful! 

aleatorio :: (Random a, Num a) => a -> a -> a
aleatorio i j = unsafePerformIO (randomRIO (i,j))

prob :: (Random a, Enum a, Fractional a) => a
prob = [0.0001,0.0002..1]!!(unsafePerformIO (randomRIO (0,9999)))    

b_escalada :: (Num b) => (() -> [a]) -> ([a] -> [a]) -> ([a] -> b) -> (b -> b -> Bool) -> ([a], b)
b_escalada genera_inicial genera_sucesor f_valoracion mejor = 
    b_escalada_aux actual sucesor genera_sucesor f_valoracion mejor
    where
        sucesor = genera_sucesor actual
        actual = genera_inicial()

b_escalada_aux :: (Num b) => [a] -> [a] -> ([a] -> [a]) -> ([a] -> b) -> (b -> b -> Bool) -> ([a], b)
b_escalada_aux actual vecino genera_sucesor f_valoracion mejor
    | mejor v_vecino v_actual = b_escalada_aux vecino sucesor genera_sucesor f_valoracion mejor
    | otherwise = (actual, v_actual)
    where
        sucesor = genera_sucesor vecino
        v_actual = f_valoracion actual
        v_vecino = f_valoracion vecino

b_escalada_reinicio :: (Num b) => (() -> [a]) -> ([a] -> [a]) -> ([a] -> b) -> (b -> b -> Bool) -> Int -> ([a], b)
b_escalada_reinicio genera_inicial genera_sucesor f_valoracion mejor n_reinicios =
    b_escalada_reinicio_aux genera_inicial genera_sucesor f_valoracion mejor (n_reinicios-1) sol
    where
        sol = b_escalada genera_inicial genera_sucesor f_valoracion mejor

b_escalada_reinicio_aux :: (Num b) => (() -> [a]) -> ([a] -> [a]) -> ([a] -> b) -> (b -> b -> Bool) -> Int -> ([a], b) -> ([a], b)
b_escalada_reinicio_aux genera_inicial genera_sucesor f_valoracion mejor n_reinicios sol
    | n_reinicios == 0   = sol
    | mejor (snd p) (snd sol) = b_escalada_reinicio_aux genera_inicial genera_sucesor f_valoracion mejor (n_reinicios-1) p
    | otherwise = b_escalada_reinicio_aux genera_inicial genera_sucesor f_valoracion mejor (n_reinicios-1) sol
    where
        p = b_escalada genera_inicial genera_sucesor f_valoracion mejor

------------------------------------------------------------------------

genera_inicial :: () -> [Int]
genera_inicial _ = [aleatorio 1 9 | _ <- [1..5]]

genera_sucesor :: [Int] -> [Int]
genera_sucesor actual = (take (i-1) actual) ++ [n] ++ (drop i actual)
    where
        i = aleatorio 1 (length actual-1)
        n = aleatorio 1 9

f_valoracion :: [Int] -> Int
f_valoracion l = sum l 

mejor :: Int -> Int -> Bool
mejor v_candidata v_actual = v_candidata > v_actual

media n i m
    | n > 0 = media (n-1) i (m + ((fromIntegral v) / (fromIntegral i))) 
    | otherwise = m
    where
        v = snd $ b_escalada_reinicio genera_inicial genera_sucesor f_valoracion mejor 1000