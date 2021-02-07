import IA
import System.Random

type Coordenada = (Double, Double)
type Ciudad = (String, Coordenada)
type Mapa = [Ciudad]

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                     Funciones auxiliares                        ║  
-- ╚═════════════════════════════════════════════════════════════════╝

mezclar :: [a] -> IO [a]
mezclar x
    | (length x < 2) = return x 
    | otherwise =  do
                    i <- randomRIO (0, length(x)-1)
                    r <- mezclar ((take i x)++(drop (i+1) x))
                    return ((x!!i):r)

mapaAndalucia :: Mapa
mapaAndalucia = [("almeria", (409.5, 93)),
                    ("cadiz", (63, 57)),
                    ("cordoba", (198, 207)),
                    ("granada", (309, 127.5)),
                    ("huelva", (3, 139.5)),
                    ("jaen", (295.5, 192)),
                    ("malaga", (232.5,  75)),
                    ("sevilla", (90, 153))]

seleccionaIndividuos :: Int -> Int -> IO (Int, Int)                      
seleccionaIndividuos mininimo maximo = do
    i <- randomRIO (mininimo, maximo)
    j <- randomRIO (mininimo, maximo)
    if i == j
        then
            seleccionaIndividuos mininimo maximo
        else
            return (min i j, max i j)

intercambiaPosiciones :: (Int, Int) -> Mapa -> Mapa
intercambiaPosiciones (a, b) xs = take (length xs) [ selecciona i | i <- [0..] ]
    where
        selecciona p
            | p == a = xs!!b
            | p == b = xs!!a
            | otherwise = xs!!p

-- intercambiaPosiciones :: (Int, Int) -> Mapa -> Mapa
-- intercambiaPosiciones (a, b) xs = [ selecciona i | i <- [0..length xs-1] ]
--     where
--         selecciona p
--             | p == a = xs!!b
--             | p == b = xs!!a
--             | otherwise = xs!!p

calculaDistancia :: Coordenada -> Coordenada -> Double
calculaDistancia (a1,a2) (b1, b2) = sqrt $ (b1-a1)**2 + (b2-a2)**2

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                  Implementación del problema                    ║  
-- ╚═════════════════════════════════════════════════════════════════╝

generaInicial :: IO Mapa
generaInicial = mezclar $ mapaAndalucia

generaSucesor :: Mapa -> IO Mapa
generaSucesor actual = do
    cortes <- seleccionaIndividuos 0 (length actual - 1)
    return $ intercambiaPosiciones cortes actual

fValoracion :: Mapa -> Double
fValoracion ls = sum $ map (\(a, b) -> calculaDistancia (snd a) (snd b)) $ ls'++[(last ls, head ls)]
    where
        ls' = take (length ls-1) [((ls!!i), (ls!!(i+1))) | i <- [0..]]
mejor :: Double -> Double -> Bool
mejor v_candidata v_actual = v_candidata < v_actual

problemaTsp :: Problema Ciudad
problemaTsp = problema (generaInicial) (generaSucesor) (fValoracion) (mejor)


-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                           Soluciones                            ║  
-- ╚═════════════════════════════════════════════════════════════════╝

solucionBusquedaEscalada :: IO (Solucion Ciudad)
solucionBusquedaEscalada = bEscalada problemaTsp

solucionBusquedaEscaladaReinicio :: IO (Solucion Ciudad)
solucionBusquedaEscaladaReinicio = bEscaladaReinicio problemaTsp 100

solucionEnfriamientoSimulado :: IO (Solucion Ciudad)
solucionEnfriamientoSimulado = eSimulado problemaTsp 100 0.1 100 100
