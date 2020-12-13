import System.Random
import System.IO.Unsafe  -- be careful!  
--import IA

--aleatorios :: (Random a, Integral a) => Int -> Int -> a
--aleatorios i j = unsafePerformIO (getStdRandom (randomR (i, j)))

--aleatorio :: Int -> Int -> Int
--aleatorio i j = unsafePerformIO (getStdRandom (randomR (i, j)))

--ab :: (Random a, Num a) => a
--ab = unsafePerformIO (randomRIO (1, 10))

--genera_inicial :: (Random a, Integral a) => [a]
--genera_inicial = [aleatorios 1 9 | _ <- [1..5]]
--[0.0001,0.0002..1]!!(unsafePerformIO (randomRIO (0,9999)))
