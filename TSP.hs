{--
Definir una función cuadrado_puntos_bl(n), que recibiendo un número natural n, devuelve una instancia 
del problema del viajante (para búsqueda local), considerando que las ciudades son  4𝑛  puntos del plano 
dispuestos en forma de cuadrado, tal y como se ha explicado en clase.
# Escribe aquí la solución

def cuadrado_puntos_bl(n):
    ciudades_coords = {}
    id_ciudad = 0
    
    for i in range(n+1):
        ciudades_coords[id_ciudad] = (0, i)
        ciudades_coords[id_ciudad+1] = (n, i)
        id_ciudad += 2
        
        if i not in (0, n):
            ciudades_coords[id_ciudad] = (i, 0)
            ciudades_coords[id_ciudad + 1] = (i, n)
            id_ciudad += 2
            
    return Viajante_BL(ciudades_coords.keys(), lambda x,y : distancia_euc2D(x,y,ciudades_coords))


andalucia={"almeria": (409.5, 93),
           "cadiz":(63, 57),
           "cordoba": (198, 207),
           "granada": (309, 127.5),
           "huelva":  (3, 139.5),
           "jaen":    (295.5, 192),
           "malaga":  (232.5,  75),
           "sevilla": ( 90, 153)}
--}

import IA
import System.Random
import Data.Array.IO
import Control.Monad

type Coordenada = (Int, Int)
type Mapa = [Coordenada]

mezclar :: [a] -> IO [a]
mezclar x
    | (length x < 2) = return x 
    | otherwise =  do
                    i <- randomRIO (0, length(x)-1)
                    r <- mezclar ((take i x)++(drop (i+1) x))
                    return ((x!!i):r)

cuadrado_n :: Int -> Mapa
cuadrado_n n = [(i, j) | i <- [0..n], j <- [0..n], i == 0 || i == n || j == 0 || j == n]

genera_inicial :: IO Mapa
genera_inicial = mezclar $ cuadrado_n 7

selecciona_individuos :: Int -> Int -> IO (Int, Int)                      
selecciona_individuos mininimo maximo = do
    i <- randomRIO (mininimo, maximo)
    j <- randomRIO (mininimo, maximo)
    if i == j
        then
            selecciona_individuos mininimo maximo
        else
            return (min i j, max i j)

genera_sucesor :: Mapa -> IO Mapa
genera_sucesor actual = do
    cortes <- selecciona_individuos 0 (length actual - 1)
    return $ intercambia_posiciones cortes actual

intercambia_posiciones :: (Int, Int) -> Mapa -> Mapa
intercambia_posiciones (a, b) xs = [ selecciona i | i <- [0..length xs-1] ]
    where
        selecciona p
            | p == a = xs!!b
            | p == b = xs!!a
            | otherwise = xs!!p

f_valoracion :: Mapa -> Double
f_valoracion ls = sum $ map (\(a, b) -> calcula_distancia a b) $ [((ls!!i), (ls!!(i+1))) | i <- [0..length ls-2]]++[(last ls, head ls)]
        

calcula_distancia :: Coordenada -> Coordenada -> Double
calcula_distancia (a1,a2) (b1, b2) = sqrt $ fromIntegral $ (b1-a1)^2 + (b2-a2)^2

mejor :: Double -> Double -> Bool
mejor v_candidata v_actual = v_candidata < v_actual

problema_tsp :: Problema Coordenada
problema_tsp = Problema (genera_inicial) (genera_sucesor) (f_valoracion) (mejor)

{-
type Ind a = [a]
type Val = Double
--type Pr a = (Solucion a, Solucion a)
type Gi a = (IO (Ind a))
type Gs a = (Ind a -> IO (Ind a))
type Fv a = (Ind a -> Val)
type Me = (Val -> Val -> Bool)

data Solucion a = Solucion (Ind a) Val
deriving Show

data Problema a = Problema (Gi a) (Gs a) (Fv a) Me 
 -}

{- 
genera_inicial_aux :: Int -> IO [Coordenada]
genera_inicial_aux a = do
                    if(a==0)
                        then do
                            x <- randomRIO (1, 9)
                            return [x]
                        else do
                            x <- randomRIO (1, 9)
                            inicial <- genera_inicial_aux (a-1)
                            return (x:inicial) -}