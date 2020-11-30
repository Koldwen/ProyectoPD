import System.Random
import System.IO.Unsafe  -- be careful!  
import IA

--aleatorio :: Int -> Int -> Int
--aleatorio i j = unsafePerformIO (randomRIO (i, j))

genera_inicial :: (Random a, Integral a) => [a]
genera_inicial = [aleatorio 1 9 | _ <- [1..5]]

genera_sucesor :: [Int] -> [Int]
genera_sucesor actual = (take (i-1) actual) ++ [n] ++ (drop i actual)
    where
        i = aleatorio 1 (length actual-1)
        n = aleatorio 1 9

f_valoracion :: [Int] -> Int
f_valoracion l = sum l 

mejor :: Int -> Int -> Bool
mejor v_candidata v_actual = v_candidata > v_actual