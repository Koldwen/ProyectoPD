import System.Random
import System.IO.Unsafe  -- be careful!  
import IA

genera_inicial :: (Random a, Integral a) => [a]
genera_inicial = [aleatorio 1 9 | _ <- [1..5]]

funcion1 :: [a] -> ([a], Int)
funcion1 gi = (gi, 3)

funcion2 :: (Random c, Integral c) => [c] -> ([a], Int)
funcion2 gi = (gi, 3)