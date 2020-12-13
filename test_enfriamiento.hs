import IA

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
{--media n i m
    | n > 0 = media (n-1) i (m + (v / i)) 
    | otherwise = m
    where
        v = snd $ e_simulado genera_inicial genera_sucesor f_valoracion mejor 100 0.9 10 10--}