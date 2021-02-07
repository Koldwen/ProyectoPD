import IA
import System.Random

type Coordenada = (Int, Int)
type Mapa = [Coordenada]

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                     Funciones auxiliares                        ║  
-- ╚═════════════════════════════════════════════════════════════════╝

calculaDistancia :: Coordenada -> Coordenada -> Double
calculaDistancia (a1,a2) (b1, b2) = sqrt $ fromIntegral $ (b1-a1)^2 + (b2-a2)^2

mezclar :: [a] -> IO [a]
mezclar x
    | (length x < 2) = return x 
    | otherwise =  do
                    i <- randomRIO (0, length(x)-1)
                    r <- mezclar ((take i x)++(drop (i+1) x))
                    return ((x!!i):r)

cuadradoN :: Int -> Mapa
cuadradoN n = [(i, j) | i <- [0..n], j <- [0..n], i == 0 || i == n || j == 0 || j == n]

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
intercambiaPosiciones (a, b) xs = [ selecciona i | i <- [0..length xs-1] ]
    where
        selecciona p
            | p == a = xs!!b
            | p == b = xs!!a
            | otherwise = xs!!p

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                  Implementación del problema                    ║  
-- ╚═════════════════════════════════════════════════════════════════╝

generaInicial :: IO Mapa
generaInicial = mezclar $ cuadradoN 5

generaSucesor :: Mapa -> IO Mapa
generaSucesor actual = do
    cortes <- seleccionaIndividuos 0 (length actual - 1)
    return $ intercambiaPosiciones cortes actual

fValoracion :: Mapa -> Double
fValoracion ls = sum $ map (\(a, b) -> calculaDistancia a b) $ [((ls!!i), (ls!!(i+1))) | i <- [0..length ls-2]]++[(last ls, head ls)]
        
mejor :: Double -> Double -> Bool
mejor vCandidata vActual = vCandidata < vActual

problemaTsp :: Problema Coordenada
problemaTsp = problema (generaInicial) (generaSucesor) (fValoracion) (mejor)

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                           Soluciones                            ║  
-- ╚═════════════════════════════════════════════════════════════════╝

solucionBusquedaEscalada :: IO (Solucion Coordenada)
solucionBusquedaEscalada = bEscalada problemaTsp

solucionBusquedaEscaladaReinicio :: IO (Solucion Coordenada)
solucionBusquedaEscaladaReinicio = bEscaladaReinicio problemaTsp 100

solucionEnfriamientoSimulado :: IO (Solucion Coordenada)
solucionEnfriamientoSimulado = eSimulado problemaTsp 100 0.1 100 100