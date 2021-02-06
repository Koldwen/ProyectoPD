import IA
import Data.List

-- PENDIENTE DE BORRAR

ejemplosIris :: [[Double]]
ejemplosIris=[
       [5.8, 2.8, 5.1, 2.4],
       [6 , 2.2, 4 , 1 ],
       [5.5, 4.2, 1.4, 0.2],
       [7.3, 2.9, 6.3, 1.8],
       [5 , 3.4, 1.5, 0.2],
       [6.3, 3.3, 6 , 2.5],
       [5 , 3.5, 1.3, 0.3],
       [6.7, 3.1, 4.7, 1.5],
       [6.8, 2.8, 4.8, 1.4],
       [6.1, 2.8, 4 , 1.3],
       [6.1, 2.6, 5.6, 1.4],
       [6.4, 3.2, 4.5, 1.5],
       [6.1, 2.8, 4.7, 1.2],
       [6.5, 2.8, 4.6, 1.5],
       [6.1, 2.9, 4.7, 1.4],
       [4.9, 3.6, 1.4, 0.1],
       [6 , 2.9, 4.5, 1.5],
       [5.5, 2.6, 4.4, 1.2],
       [4.8, 3 , 1.4, 0.3],
       [5.4, 3.9, 1.3, 0.4],
       [5.6, 2.8, 4.9, 2 ],
       [5.6, 3 , 4.5, 1.5],
       [4.8, 3.4, 1.9, 0.2],
       [4.4, 2.9, 1.4, 0.2],
       [6.2, 2.8, 4.8, 1.8],
       [4.6, 3.6, 1 , 0.2],
       [5.1, 3.8, 1.9, 0.4],
       [6.2, 2.9, 4.3, 1.3],

       [5 , 2.3, 3.3, 1],
       [5 , 3.4, 1.6, 0.4],
       [6.4, 3.1, 5.5, 1.8],
       [5.4, 3 , 4.5, 1.5],
       [5.2, 3.5, 1.5, 0.2],
       [6.1, 3 , 4.9, 1.8],
       [6.4, 2.8, 5.6, 2.2],
       [5.2, 2.7, 3.9, 1.4],
       [5.7, 3.8, 1.7, 0.3],
       [6 , 2.7, 5.1, 1.6],
       [5.9, 3 , 4.2, 1.5],
       [5.8, 2.6, 4 , 1.2],
       [6.8, 3 , 5.5, 2.1],
       [4.7, 3.2, 1.3, 0.2],
       [6.9, 3.1, 5.1, 2.3],
       [5 , 3.5, 1.6, 0.6],
       [5.4, 3.7, 1.5, 0.2],
       [5 , 2 , 3.5, 1 ],
       [6.5, 3 , 5.5, 1.8],
       [6.7, 3.3, 5.7, 2.5],
       [6 , 2.2, 5 , 1.5],
       [6.7, 2.5, 5.8, 1.8],
       [5.6, 2.5, 3.9, 1.1],
       [7.7, 3 , 6.1, 2.3],
       [6.3, 3.3, 4.7, 1.6],
       [5.5, 2.4, 3.8, 1.1],
       [6.3, 2.7, 4.9, 1.8],
       [6.3, 2.8, 5.1, 1.5],
       [4.9, 2.5, 4.5, 1.7],
       [6.3, 2.5, 5 , 1.9],
       [7 , 3.2, 4.7, 1.4],
       [6.5, 3 , 5.2, 2 ],
       [6 , 3.4, 4.5, 1.6],
       [4.8, 3.1, 1.6, 0.2],
       [5.8, 2.7, 5.1, 1.9],
       [5.6, 2.7, 4.2, 1.3],
       [5.6, 2.9, 3.6, 1.3],
       [5.5, 2.5, 4 , 1.3],
       [6.1, 3 , 4.6, 1.4],
       [7.2, 3.2, 6 , 1.8],
       [5.3, 3.7, 1.5, 0.2],
       [4.3, 3 , 1.1, 0.1],
       [6.4, 2.7, 5.3, 1.9],
       [5.7, 3 , 4.2, 1.2],
       [5.4, 3.4, 1.7, 0.2],
       [5.7, 4.4, 1.5, 0.4],
       [6.9, 3.1, 4.9, 1.5]]

clasesIris :: [Double]
clasesIris=[2, 1, 0, 2, 0, 2, 0, 1, 1, 1, 2, 1, 1, 1, 1, 0, 1, 1, 0, 0, 2, 1,
       0, 0, 2, 0, 0, 1, 1, 0, 2, 1, 0, 2, 2, 1, 0, 1, 1, 1, 2, 0, 2, 0,
       0, 1, 2, 2, 2, 2, 1, 2, 1, 1, 2, 2, 2, 2, 1, 2, 1, 0, 2, 1, 1, 1,
       1, 2, 0, 0, 2, 1, 0, 0, 1]

claseIris' :: [Double] -> [[Double]]
claseIris' [] = [] 
claseIris' (l:ls)
       | l == 0 = [[1,0,0]]++(claseIris' ls)
       | l == 1 = [[0,1,0]]++(claseIris' ls)
       | otherwise  = [[0,0,1]]++(claseIris' ls)

clasesIris' :: [[Double]]
clasesIris' = claseIris' clasesIris

conjEntrenamiento :: [([Double], [Double])]
conjEntrenamiento = [(ejemplosIris!!i, clasesIris'!!i)  | i <- [0..(length ejemplosIris)-1]]  

pesosIniciales :: Double
pesosIniciales = 1
entradasFicticias :: Double
entradasFicticias = 1
neuronasPorCapa :: [Int]
neuronasPorCapa = [4,1,1,1,1,1,3]
funcionesDeActivacion :: [Double -> Double]
funcionesDeActivacion = [sigmoide, sigmoide,sigmoide, sigmoide,sigmoide, sigmoide]
funcionesDerivadas :: [Double -> Double]
funcionesDerivadas = [dSigmoide, dSigmoide, dSigmoide, dSigmoide, dSigmoide, dSigmoide]

aciertosRed :: Red -> [([Double], [Double])] -> Int 
aciertosRed r ls = sum [ if (res x) == (obtenerClase l) then 1 else 0 | (x,l) <- ls]
       where
              res x = snd $ last $ sort (zip (predecir r x) [0..])
              obtenerClase [1,0,0] = 0
              obtenerClase [0,1,0] = 1
              obtenerClase [0,0,1] = 2




redIris :: Red
redIris = crear pesosIniciales entradasFicticias neuronasPorCapa funcionesDeActivacion funcionesDerivadas

estadisticas :: Int -> Double -> IO()
estadisticas epochs lr = do
    let redIris' = retropropagacion redIris conjEntrenamiento epochs lr 
    putStrLn $ "╔═════════════════════════════════════════╗"
    putStrLn $ "║   Datos del conjunto de entrenamiento   ║"
    putStrLn $ "╚═════════════════════════════════════════╝"
    putStrLn $ "Número de ejemplos: " ++ (show (length ejemplosIris))
    putStrLn $ "═══════════════════════════════════════════\n"
    putStrLn $ "╔═════════════════════════════════════════╗"
    putStrLn $ "║          Datos de la red inicial        ║"
    putStrLn $ "╚═════════════════════════════════════════╝"
    putStrLn $ "Pesos iniciales: " ++ (show pesosIniciales)
    putStrLn $ "Entradas ficticias: " ++ (show entradasFicticias)
    putStrLn $ "Neuronas por capa: " ++ (show neuronasPorCapa)
    putStrLn $ "Tasa de aprendizaje: " ++ (show lr)
    putStrLn $ "Número de épocas: " ++ (show epochs)
    putStrLn $ "═══════════════════════════════════════════\n"
    putStrLn $ "╔═════════════════════════════════════════╗"
    putStrLn $ "║           Número de aciertos            ║ "
    putStrLn $ "╚═════════════════════════════════════════╝"
    putStrLn $ "Antes del entrenamiento: " ++ (show (aciertosRed redIris conjEntrenamiento))
    putStrLn $ "Después del entrenamiento: " ++ (show (aciertosRed redIris' conjEntrenamiento))
    putStrLn $ "═══════════════════════════════════════════"
    return ()