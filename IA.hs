-- Proyecto PD.
-- Búsqueda en Escalada con reinicio
-- Enfriamiento Simulado
-- Redes Neuronales
-- ----------------
-- ----------------
-- ARCHIVO: IA
module IA 
(
    Ind,
    Val,

    Gi,
    Gs,
    Fv,
    Me,
    Solucion,
    Problema,
    Perceptron,
    Red,
    problema,
    red,
    sigmoide,
    relu,
    dSigmoide,
    dRelu,
    bEscalada,
    bEscaladaReinicio,
    eSimulado,
    predecir,
    retropropagacion
) where
    
import Prelude as P
import System.Random ( Random(randomRIO) )
import Data.Vector as V ( Vector, (!), fromList, length, map, toList )
import Data.Array as A

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║  Tipos usados en Búsqueda en Escalada y Enfriamiento Simulado   ║
-- ╚═════════════════════════════════════════════════════════════════╝

type Ind a = [a]                    -- Individuo de una población.
type Val = Double                   -- Valoración de un individuo.
type Gi a = (IO (Ind a))            -- Función de generación inicial de un individuo.
type Gs a = (Ind a -> IO (Ind a))   -- Función de generación de un sucesor a partir de un individuo.
type Fv a = (Ind a -> Val)          -- Función de valoración de un individuo.
type Me = (Val -> Val -> Bool)      -- Función para decidir si el primer individuo es mejor que el segundo.

data Solucion a = Solucion (Ind a) Val  -- Representa la solución de un problema, 
                                        -- y contiene al mejor individuo (solución) junto con su valoración.

instance (Show a) => Show (Solucion a) where
    show (Solucion i v) = "Solución: "P.++(show i)P.++"\nValoración: "P.++(show v)

data Problema a = Problema (Gi a) (Gs a) (Fv a) Me  -- Representa un problema, 
                                                    -- con las funciones necesarias para resolverlo.



-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                Tipos usados en Redes Neuronales                 ║
-- ╚═════════════════════════════════════════════════════════════════╝

data Perceptron = P {   
                        eFicticia :: Double,                -- Entrada ficticia del perceptrón.
                        pesos :: [Double],                  -- Pesos del perceptrón con la capa siguiente (y su peso ficticio).
                        fActivacion :: (Double -> Double),  -- Función de activación del perceptrón (una por capa, sin contar la de entrada).
                        fDerivada :: (Double -> Double)     -- Funciones de activación derivadas (una por capa, sin contar la de entrada).
                    }

instance Show Perceptron where
    show (P e ws _ _) = "(a0 = "P.++(show e)P.++", ws = "P.++(show ws)++")"
    
data Red    = E (Vector Perceptron) Red     -- Capa de entrada (Sólo se usan los pesos).
            | O (Vector Perceptron) Red     -- Capa oculta.
            | S (Vector Perceptron)         -- Capa de salida.

instance Show Red where
    show (E ps r) = mostrarCapa ps ++ "=====================\n" ++ show r
    show (O ps r) = mostrarCapa ps ++ "=====================\n" ++ show r
    show (S ps) = mostrarCapa ps

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                      Funciones auxiliares                       ║
-- ╚═════════════════════════════════════════════════════════════════╝

prob :: IO Double   -- Método
prob =  do
            xs <- (randomRIO (1, 10000))
            return (xs/10000)

sorteo :: Double -> Double -> Double -> IO Bool             
sorteo vc va t = do
    let p = exp(-abs( (vc-va) / t))
    r <- prob
    return (r <= p)

umbral :: Double -> Double
umbral x
    |  x > 0 = 1
    |  x <= 0 = 0

mostrarCapa :: Vector Perceptron -> String
mostrarCapa = mostrarCapaAux 0

mostrarCapaAux :: Int -> Vector Perceptron -> String
mostrarCapaAux i ps
    | V.length ps > i = (show (ps V.! i)) ++ "\n" ++ (mostrarCapaAux (i+1) ps)
    | otherwise = ""


-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                          Constructores                          ║
-- ╚═════════════════════════════════════════════════════════════════╝

problema :: Gi a -> Gs a -> Fv a -> Me -> Problema a
problema gi gs fv me = Problema gi gs fv me

red :: Double -> Double -> [Int] -> [Double -> Double] -> [Double -> Double] -> Red
red w a (c:cs) fs ds = E (V.fromList [ P 0 [ w | _ <- [1..p_sup]] id id | _ <- [1..c]]) (redAux w a cs fs ds)
    where 
        p_sup = P.head cs

redAux :: Double -> Double -> [Int] -> [Double -> Double] -> [Double -> Double] -> Red
redAux w a cs (f:fs) (d:ds) = 
    case cs of
        [c] ->  S (V.fromList [ P a [ w ] f d | _ <- [1..c]])
        (c:cs) -> O (V.fromList [ (P a [ w  |  _ <- [0..p_sup]] f d) | _ <- [1..c] ]) (redAux w a cs fs ds)
            where
                p_sup = P.head cs

redToArray :: Red -> Array Int (Vector Perceptron)
redToArray red = A.listArray (1, P.length capasRed) capasRed 
    where
        capasRed = capas red
        capas (S ps) = [ps]
        capas (O ps r) = [ps] ++ capas r
        capas (E ps r) = [ps] ++ capas r

arrayToRed :: Array Int (Vector Perceptron) -> Red
arrayToRed ls = E vs (arrayToRedAux vss)
    where
        (vs:vss) = A.elems ls
arrayToRedAux :: [Vector Perceptron] -> Red
arrayToRedAux ls = 
    case ls of
        [l]    -> S l
        (l:ls) -> O l (arrayToRedAux ls)
-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                    Funciones de activación                      ║
-- ╚═════════════════════════════════════════════════════════════════╝

sigmoide :: Double -> Double
sigmoide x = 1 / (1+exp(-x))

relu :: Double -> Double
relu = max 0


-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                Funciones de activación derivadas                ║
-- ╚═════════════════════════════════════════════════════════════════╝

dSigmoide :: Double -> Double
dSigmoide x = sigmoide x * (1-sigmoide x)

dRelu :: Double -> Double
dRelu x = umbral x



-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                 Algoritmo Búsqueda en Escalada                  ║  
-- ╚═════════════════════════════════════════════════════════════════╝

-- Método principal para el algoritmo de búsqueda en escalada.
-- Parámetros: Problema a resolver -> Mejor estado encontrado (solución)
bEscalada :: Problema a -> IO (Solucion a)
bEscalada p@(Problema gi gs fv me) = do
    i <- gi                                 -- Generamos estado inicial.
    s <- gs i                               -- Generamos un sucesor a partir del estado inicial.
    let sucesor = Solucion s (fv s)         -- Creamos un nuevo tipo Solución apartir del estado sucesor.
    let actual = Solucion i (fv i)          -- Creamos un nuevo tipo Solución apartir del estado actual.
    bEscaladaAux p actual sucesor           -- Aplicamos el algoritmo.

-- Método auxiliar para el algoritmo de búsqueda en escalada.
-- Parámetros: Problema a resolver -> Estado actual -> Estado sucesor -> Mejor estado encontrado (solución)
bEscaladaAux :: Problema a -> Solucion a -> Solucion a -> IO (Solucion a)
bEscaladaAux p@(Problema _ gs fv me) actual@(Solucion _ vAct) sucesor@(Solucion iSuc vSuc) = do
    if me (vSuc) (vAct)                                         -- Comprobamos si el estado sucesor es mejor que el estado actual apartir de la función mejor.
        then do
            iSucesor' <- gs $ iSuc                              -- Generamos un sucesor apartir del estado sucesor que ya disponiamos.
            let sucesor' = Solucion iSucesor' (fv iSucesor')    -- Creamos una nueva Solución a partir del nuevo sucesor.
            bEscaladaAux p sucesor sucesor'                     -- Si el estado sucesor es mejor que el actual, volvemos a ejecutar el algoritmo.
        else
            return actual                                       -- Si el estado sucesor no es mejor que el actual, devolvemos el estado actual como solución.
-- ╔═════════════════════════════════════════════════════════════════╗
-- ║         Algoritmo Búsqueda en Escalada con Reinicio             ║  
-- ╚═════════════════════════════════════════════════════════════════╝

-- Método principal para el algoritmo de búsqueda en escalada con reinicio.
-- Parámetros: Problema a resolver -> Número de reinicios -> Mejor estado encontrado (solución)
bEscaladaReinicio :: Problema a -> Int -> IO (Solucion a)
bEscaladaReinicio p nReinicios = do
    if nReinicios <= 0                                      -- Comprobamos que el número de reinicios sea mayor que 0.        then
        then
            error "El número de reinicios debe ser mayor o igual a 1."
        else do
            actual <- bEscalada p                           -- Realizamos el algorimo de búsqueda en escalada.
            bEscaladaReinicioAux p (nReinicios-1) actual    -- Ejecutamos el algoritmo de búsqueda en escalada con reinicio.

-- Método auxiliar para el algoritmo de búsqueda en escalada con reinicio.
-- Parámetros: Problema a resolver -> Número de reinicios -> Estado actual -> Mejor estado encontrado (solución)
bEscaladaReinicioAux :: Problema a -> Int -> Solucion a -> IO (Solucion a)
bEscaladaReinicioAux _ 0 sol = return sol                   -- Caso base
bEscaladaReinicioAux p@(Problema _ _ _ me) nReinicios actual@(Solucion _ vAct) = do
    sucesor@(Solucion _ vSuc) <- bEscalada p             -- Generamos un sucesor a partir del estado actual. 
    if me (vSuc) (vAct)                                     -- Comprobamos si el nuevo estado sucesor es mejor que el estado actual apartir de la función mejor
        then 
            bEscaladaReinicioAux p (nReinicios-1) sucesor   -- Si el sucesor es mejor que el actual ejecutamos otra vez el algoritmo utizando el sucesor
        else
            bEscaladaReinicioAux p (nReinicios-1) actual    -- Si el actual es mejor que el sucesor ejecutamos otra vez el algoritmo utizando el actual

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                Algoritmo Enfriamiento Simulado                  ║  
-- ╚═════════════════════════════════════════════════════════════════╝

-- Método principal para el algoritmo de enfriamiento simulado.
-- Parámetros: Problema a resolver -> Temperatura -> Factor descenso -> Número de enfriamientos -> Número de iteraciones -> Mejor estado encontrado (solución)
eSimulado :: Problema a -> Double -> Double -> Int -> Int -> IO (Solucion a)
eSimulado p@(Problema gi gs fv me) t d ne ni = do
    if ne <= 0                                                  -- Comprobamos si el número de enfriamientos es mayor que 0
        then
            error "El número de enfriamientos debe ser mayor que cero."
        else
            if ni <= 0                                          -- Comprobamos si el número de iteraciones es mayor que 0
                then
                    error "El número de iteraciones debe ser mayor que cero."
                else do
                    iAct <- gi                                  -- Generamos estado inicial
                    let actual = (Solucion iAct (fv iAct))
                    eSimuladoEnfr p t d ne ni actual actual     -- Ejecutamos el algoritmo

-- Bucle de enfriamientos para el algoritmo de enfriamiento simulado.
-- Parámetros: Problema a resolver -> Temperatura -> Factor descenso -> Número de enfriamientos -> Número de reinicios -> Estado actual -> Mejor estado encontrado -> Mejor estado encontrado (solución)
eSimuladoEnfr :: Problema a -> Double -> Double -> Int -> Int -> Solucion a -> Solucion a -> IO (Solucion a)
eSimuladoEnfr _ _ _ 0 _ _ mejor = return mejor                  -- Caso base, devolvemos el mejor estado encontrado.
eSimuladoEnfr p t d ne ni actual mejor = do
    let t' = t * d                                              -- Actualizamos la temperatura con el factor descenso.
    let ne' = ne-1                                              -- Disminuimos el número de enfriamientos.
    (actual',mejor') <- eSimuladoIter p t' ni actual mejor      -- Creamos una tupla con el estado actual y el mejor estado conseguido hasta el momento.
    eSimuladoEnfr p t' d ne' ni actual' mejor'                  -- Volvemos a ejecutar el algoritmo con los valores actualizados

-- Bucle de iteraciones para el algoritmo de enfriamiento simulado.
-- Parámetros: Problema a resolver -> Temperatura -> Número de iteraciones -> Estado actual -> Mejor estado encontrado -> Mejor estado encontrado (solución)
eSimuladoIter :: Problema a -> Double -> Int -> Solucion a-> Solucion a -> IO (Solucion a, Solucion a)
eSimuladoIter _ _ 0 actual mejor = return (actual, mejor)       -- Caso base, devolvemos el mejor estado encontrado.
eSimuladoIter p@(Problema _ gs fv me) t ni actual@(Solucion iAct vAct) mejor@(Solucion _ vMej) = do
    iSuc <- gs iAct                                             -- Generamos sucesor
    let sucesor@(Solucion _ vSuc') = Solucion iSuc (fv iSuc)
    let incremento = vSuc' - vAct
    let mejor' = if me vSuc' vMej then sucesor else mejor       -- Escogemos el mejor
    let ni' = ni - 1                                            -- Decrementamos el número de iteraciones
    ganaSorteo <- sorteo vSuc' vAct t                           -- Realizamos el sorteo
    let aceptarCandidata = incremento < 0 || ganaSorteo         -- Escogemos si el candidato es aceptado
    if aceptarCandidata
        then
            eSimuladoIter p t ni' sucesor mejor'                -- Volvemos a ejecutar el algoritmo utilizando el sucesor como actual
        else
            eSimuladoIter p t ni' actual mejor'                 -- Volvemos a ajecutar el algoritmo sin cambiar el actual


-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                        Redes Neuronales                         ║  
-- ╚═════════════════════════════════════════════════════════════════╝

-- Parámetros: Red -> Valores para la capa de entrada -> Valores obtenidos por la capa de salida.
predecir :: Red -> [Double] -> [Double]
predecir (S v) ls = salida      -- Capa de salida de la red
    where
        salida = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
                                -- Calculamos el resultado de aplicar la función de activación a cada uno de los valores que llegan a la capa

predecir (O v r) ls = predecir r salida                     -- Capas ocultas de la red
    where
        salida = [ calculaSalida i |  i <- [0..nPesos-1]]   -- Calculamos las salidas de cada perceptrón
        nPesos = (P.length (pesos (v V.! 0))) - 1  
        ls' = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
                                                            -- Calculamos el resultado de aplicar la función de activación a cada uno de los valores que llegan a la capa.
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls' $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]
                                                            -- Realizamos la suma de la multiplicación de cada uno de los valores que llegan con sus correspondientes pesos

predecir (E v r) ls = predecir r salida                     -- Capa de entrada de la red
    where
        salida = [ calculaSalida i |  i <- [0..nPesos-1]]   -- Calculamos las salidas de cada perceptrón
        nPesos = P.length $ pesos (v V.! 0)
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]
                                                            -- Realizamos la suma de la multiplicación de cada uno de los valores que llegan con sus correspondientes pesos

-- Método de propagación hacia adelante para el algoritmo de retropropagación.
-- Parámetros: Red -> Lista de elementos de entrada -> Asociación de perceptrones con sus inputs
propHaciaAdelante :: Red -> [Double] -> [[(Perceptron, Double)]]
propHaciaAdelante (S v) ls = [resultado]                            -- Capa de salida
    where
        resultado = P.zip (V.toList v) ls                           -- Creamos una lista de tuplas con el perceptrón y su input
        
propHaciaAdelante (O v r) ls = resultado : (propHaciaAdelante r salida) -- Capa oculta
    where
        resultado = P.zip (V.toList v) ls                               -- Creamos una lista de tuplas con el perceptrón y su input
        salida = [ calculaSalida i |  i <- [0..nPesos-1]]               -- Calculamos la salida para pasársela a la siguiente capa
        nPesos = (P.length (pesos (v V.! 0))) - 1
        ls' = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
                                                -- Aplicamos la función de activación a la entrada de los perceptrones.
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls' $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]
                                                -- Realizamos la suma de la multiplicación de cada uno de los valores que llegan con sus correspondientes pesos

propHaciaAdelante (E v r) ls = resultado : (propHaciaAdelante r salida)   -- Capa de entrada
    where
        resultado = P.zip (V.toList v) ls                                 -- Creamos una lista de tuplas con el perceptrón y su input
        salida = [ calculaSalida i |  i <- [0..nPesos-1]]                 -- Calculamos la salida para pasarsela a la siguiente capa
        nPesos = P.length $ pesos (v V.! 0)
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]
                                                -- Realizamos la suma de la multiplicación de cada uno de los valores que llegan con sus correspondientes pesos

-- Método auxiliar para la propagación hacia atrás (se usa para las capas ocultas y de salida).
-- Parámetros: Asociación de perceptrones con sus inputs -> Salida esperada -> Factor de aprendizaje -> Tupla con las variaciones y la red obtenida
propHaciaAtrasAux :: [[(Perceptron, Double)]] -> [Double] -> Double -> ([Double], Red)
propHaciaAtrasAux [ls] ys lr = (variaciones, capa)                                  -- Capa de salida
    where
        variaciones = P.map (\i -> derivada i * diferencia i) [0..(P.length ls)-1]  -- Calculamos las variaciones que se producen respecto a la salida esperada
        derivada i = fDerivada p inp                                                -- Función auxiliar para calcular la derivada de un input y un perceptrón dado un índice.
            where
                p = fst (ls!!i)
                inp = snd (ls!!i) + (P.head (pesos p)) * (eFicticia p)
        diferencia i = ys!!i - (fActivacion p inp)                                  -- Función auxiliar para calcular la diferencia respecto a la salida esperada y la salida obtenida dado un índice.
            where
                p = fst (ls!!i)
                inp = snd (ls!!i) + (P.head (pesos p)) * (eFicticia p)
        pesosAct = [P.head(pesos p) + lr*(eFicticia p)*(variaciones!!i) | i <- [0..(P.length ls)-1], let p = fst(ls!!i)]                -- Actualizamos los pesos de los perceptrones
        capa = S (V.fromList [p {pesos = (pesosAct!!i):(P.tail ws)} | i <- [0..(P.length ls)-1], let p = fst(ls!!i), let ws = pesos p]) -- Creamos una capa con los pesos actulizados

propHaciaAtrasAux (l:ls) ys lr = (variaciones, capa)                                -- Capas ocultas
    where
        variaciones = P.map (\i -> derivada i * sumatorio i) [0..(P.length l)-1]    -- Calculamos las variaciones que se producen respecto a la salida esperada
        derivada i = fDerivada p inp                                                -- Función auxiliar para calcular la derivada de un input y un perceptrón dado un índice.
            where
                p = fst (l!!i)
                inp = snd (l!!i) + (P.head (pesos p)) * (eFicticia p)
        sumatorio i = P.sum $ P.map (\(a,b) -> a*b) (P.zip (P.tail (pesos p)) (variaciones')) -- Función auxiliar para calcular el sumatorio de los errores cometidos dado un índice.
            where
                a = eFicticia p
                p = fst (l!!i)
        resultado = propHaciaAtrasAux ls ys lr                                      -- Aplicamos el algoritmo para las siguientes capas
        variaciones' = fst resultado                                                -- Variaciones de las siguientes capas
        red' = snd resultado                                                        -- Red creada a partir de las siguientes capas
        actualizarPesos i =                                                         -- Función auxiliar para actualizar los pesos de la capa respecto a las variaciones de las siguientes dado un índice.
            (ws!!0 + lr*(es!!0)*(vs!!0)):(P.map (\j -> ws!!j + lr*(es!!1)*(vs!!j)) [1..(P.length ws)-1])
            where
                p = fst (l!!i) 
                ws = pesos p
                es = [eFicticia p, fActivacion p (snd (l!!i) + (P.head (pesos p)) * (eFicticia p))]
                vs = (variaciones!!i) : variaciones'
        pesosAct = [ actualizarPesos i | i <- [0..(P.length l)-1]]                  -- Actualizamos los pesos
        perceptrones' = V.fromList  [ p {pesos = pesosAct!!i} |  i <- [0..(P.length pesosAct)-1], let p = fst(l!!i)]
        capa = O perceptrones' (snd resultado)                                      -- Creamos una red actualiada a partir de las capas posteriores 


-- Método de propagación hacia atrás para el algoritmo de retropropagación.
-- Parámetros: Asociación de perceptrones con sus inputs -> Salida esperada -> Factor de aprendizaje -> Red obtenida
propHaciaAtras :: [[(Perceptron, Double)]] -> [Double] -> Double -> Red
propHaciaAtras (l:ls) ys lr = red -- Capa de entrada
    where
        resultado = propHaciaAtrasAux ls ys lr  -- Aplicamos el algoritmo para las siguientes capas
        variaciones' = fst resultado            -- Variaciones obtenidas de las siguientes capas
        red' = snd resultado                    -- Red obtenida de las siguientes capas
        red = E perceptrones' red'              -- Creación de la nueva red completa
        actualizarPesos i =                     -- Función auxiliar para actualizar los pesos de la capa respecto a las variaciones de las siguientes dado un índice.
            (P.map (\j -> ws!!j + lr*(es)*(vs!!j)) [0..(P.length ws)-1])
            where
                p = fst (l!!i)
                ws = pesos p
                es = snd (l!!i)
                vs = variaciones'
        pesosAct = [ actualizarPesos i | i <- [0..(P.length l)-1]]          -- Actualizamos los pesos.
        perceptrones' = V.fromList  [ p {pesos = pesosAct!!i} |  i <- [0..(P.length pesosAct)-1], let p = fst(l!!i)] 
                                                -- Creamos una red actualiada a partir de las capas posteriores 


-- Método principal del algoritmo de retropropagación.
-- Parámetros: Red -> Conjunto de entrenamiento (Valores introducidos, Valores esperados) -> Epochs -> Factor de aprendizaje -> Red
retropropagacion :: Red -> [([Double], [Double])] -> Int -> Double -> Red
retropropagacion red entrenamiento 0 lr = red                       -- Caso base
retropropagacion red entrenamiento epochs lr
    | epochs <= 0   = error "El número de épocas debe ser mayor que cero."
    | otherwise     = retropropagacion red' entrenamiento (epochs-1) lr -- Realizamos retropropagación con la nueva red y decrementamos el número de epochs
    where
        red' = retropropagacionAux red entrenamiento lr

-- Método auxiliar para el algoritmo de retropropagación.
-- Parámetros: Red -> Conjunto de entrnamiento (Valores introducidos, Valores esperados) -> Factor de aprendizaje -> Red
retropropagacionAux :: Red -> [([Double], [Double])] -> Double -> Red
retropropagacionAux red entr lr = 
    case entr of
        [] -> red -- Caso base
        (e:entrenamiento) -> retropropagacionAux red' entrenamiento lr    -- Realizamos la retropropagación con cada uno de los ejemplos del conjunto de entrenamiento
            where
                propDelante = propHaciaAdelante red (fst e)                 -- Realizamos la propagación hacia adelante
                red' = propHaciaAtras propDelante (snd e) lr                -- Realizamos la propagación hacia atrás

