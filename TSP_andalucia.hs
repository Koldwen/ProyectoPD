import IA
import System.Random
import GHC.Float

type Coordenada = (Double, Double)
type Ciudad = (String, Coordenada)
type Mapa = [Ciudad]

mezclar :: [a] -> IO [a]
mezclar x
    | (length x < 2) = return x 
    | otherwise =  do
                    i <- randomRIO (0, length(x)-1)
                    r <- mezclar ((take i x)++(drop (i+1) x))
                    return ((x!!i):r)

mapa_andalucia :: Mapa
mapa_andalucia = [("almeria", (409.5, 93)),
                    ("cadiz", (63, 57)),
                    ("cordoba", (198, 207)),
                    ("granada", (309, 127.5)),
                    ("huelva", (3, 139.5)),
                    ("jaen", (295.5, 192)),
                    ("malaga", (232.5,  75)),
                    ("sevilla", (90, 153))]

genera_inicial :: IO Mapa
genera_inicial = mezclar $ mapa_andalucia

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
f_valoracion ls = sum $ map (\(a, b) -> calcula_distancia (snd a) (snd b)) $ [((ls!!i), (ls!!(i+1))) | i <- [0..length ls-2]]++[(last ls, head ls)]

calcula_distancia :: Coordenada -> Coordenada -> Double
calcula_distancia (a1,a2) (b1, b2) = sqrt $ (b1-a1)**2 + (b2-a2)**2

mejor :: Double -> Double -> Bool
mejor v_candidata v_actual = v_candidata < v_actual

problema_tsp :: Problema Ciudad
problema_tsp = Problema (genera_inicial) (genera_sucesor) (f_valoracion) (mejor)
