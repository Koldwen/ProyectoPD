import IA

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