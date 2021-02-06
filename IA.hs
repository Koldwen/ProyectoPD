-- Proyecto PD.
-- Búsqueda en Escalada con reinicio
-- Enfriamiento Simulado
-- Algoritmo Genético
-- PSO
-- Redes Neuronales
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
    sigmoide,
    relu,
    dSigmoide,
    dRelu,
    bEscalada,
    bEscaladaReinicio,
    eSimulado,
    predecir,
    crear,
    retropropagacion
) where
    
import Prelude as P
import System.Random ( Random(randomRIO) )
import Data.Vector as V ( Vector, (!), fromList, length, map, toList )

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

prob :: IO Double
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

bEscalada :: Problema a -> IO (Solucion a)
bEscalada p@(Problema gi gs fv me) = do
    i <- gi                                 -- Generamos estado inicial
    s <- gs i -- Generamos un sucesor        apartir del estado inicial
    let sucesor = Solucion s (fv s)           -- Creamos un nuevo tipo Solución apartir del estado inicial
    let actual = Solucion i (fv i)          -- Creamos un nuevo tipo Solución apartir del estado sucesor
    bEscaladaAux p actual sucesor  -- Aplicamos el algoritmo

bEscaladaAux :: Problema a -> Solucion a -> Solucion a -> IO (Solucion a)
bEscaladaAux p@(Problema _                  gs fv me) actual@(Solucion iAct vAct) sucesor@(Solucion iSuc vSuc) = do
    iSucesor' <- gs $ iSuc -- Generamos un sucesor apartir del estado sucesor que ya disponiamos
    let sucesor' = Solucion iSucesor' (fv iSucesor')
    if me (vSuc) (vAct) -- Comprobamos si el nuevo estado sucesor es mejor que el estado actual apartir de la función mejor
        then
            bEscaladaAux p sucesor sucesor' -- Si mejora volvemos a ejecutar el algoritmo
        else
                                   return actual -- Si el estado no ha mejorado al actual devolvemos el estado actual como solución
-- ╔═════════════════════════════════════════════════════════════════╗
-- ║         Algoritmo Búsqueda en Escalada con Reinicio             ║  
-- ╚═════════════════════════════════════════════════════════════════╝

bEscaladaReinicio :: Problema a -> Int -> IO (Solucion a)
bEscaladaReinicio p@(Problema _ gs fv me) nReinicios = do
    if nReinicios <= 0
        then
            error "El número de reinicios debe ser mayor o igual a 1."
        else do
            actual <- bEscalada p                           -- Realizamos el algorimo de búsqueda
            bEscaladaReinicioAux p (nReinicios-1) actual    -- Disminuimos el número de reinicios llamamos a la función auxiliar

bEscaladaReinicioAux :: Problema a -> Int -> Solucion a -> IO (Solucion a)
bEscaladaReinicioAux _ 0 sol = return sol                   -- Caso base
bEscaladaReinicioAux p@(Problema _ gs fv me) nReinicios actual@(Solucion iAct vAct) = do
    sucesor@(Solucion iSuc vSuc) <- bEscalada p             -- Generamos sucesor 
    if me (vSuc) (vAct)                                     -- Comprobamos si el nuevo estado sucesor es mejor que el estado actual apartir de la función mejor
        then 
            bEscaladaReinicioAux p (nReinicios-1) sucesor   -- Si el sucesor es mejor que el actual ejecutamos otra vez el algoritmo utizando el sucesor
        else
            bEscaladaReinicioAux p (nReinicios-1) actual    -- Si el actual es mejor que el sucesor ejecutamos otra vez el algoritmo utizando el actual

-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                Algoritmo Enfriamiento Simulado                  ║  
-- ╚═════════════════════════════════════════════════════════════════╝

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

eSimuladoEnfr :: Problema a -> Double -> Double -> Int -> Int -> Solucion a -> Solucion a -> IO (Solucion a)
eSimuladoEnfr _ _ _ 0 _ _ mejor= return mejor                   -- Caso base
eSimuladoEnfr p t d ne ni actual mejor = do
    let t' = t * d                                              -- Nueva temperatura
    let ne' = ne-1                                              -- Disminuimos el número de enfriamientos
    (actual',mejor') <- eSimuladoIter p t' ni actual mejor      -- Creamos una tupla con el estado actual y el mejor conseguido hasta el momento
    eSimuladoEnfr p t' d ne' ni actual' mejor'                  -- Volvemos a ejecturar el algoritmo con los valores actualizados

eSimuladoIter :: Problema a -> Double -> Int -> Solucion a-> Solucion a -> IO (Solucion a, Solucion a)
eSimuladoIter _ _ 0 actual mejor = return (actual, mejor) -- Caso base
eSimuladoIter p@(Problema _ gs fv me) t ni actual@(Solucion iAct vAct) mejor@(Solucion iMej vMej) = do
    iSuc <- gs iAct
    let sucesor@(Solucion iSuc' vSuc') = Solucion iSuc (fv iSuc)
    let incremento = vSuc' - vAct
    let mejor' = if me vSuc' vMej then sucesor else mejor
    let ni' = ni - 1
    ganaSorteo <- sorteo vSuc' vAct t
    let aceptarCandidata = incremento < 0 || ganaSorteo
    if aceptarCandidata
        then
            eSimuladoIter p t ni' sucesor mejor'
        else
            eSimuladoIter p t ni' actual mejor'


-- ╔═════════════════════════════════════════════════════════════════╗
-- ║                        Redes Neuronales                         ║  
-- ╚═════════════════════════════════════════════════════════════════╝

crear :: Double -> Double -> [Int] -> [Double -> Double] -> [Double -> Double] -> Red
crear w a (c:cs) fs ds = E (V.fromList [ P 0 [ w | _ <- [1..p_sup]] id id | _ <- [1..c]]) (crear' w a cs fs ds)
    where 
        p_sup = P.head cs

crear' :: Double -> Double -> [Int] -> [Double -> Double] -> [Double -> Double] -> Red
crear' w a [c] (f:fs) (d:ds) = S (V.fromList [ P a [ w ] f d | _ <- [1..c]])
crear' w a (c:cs) (f:fs) (d:ds) = O (V.fromList [ (P a [ w  |  _ <- [0..p_sup]] f d) | _ <- [1..c] ]) (crear' w a cs fs ds)
    where 
        p_sup = P.head cs

-- mostrar :: Red -> IO()
-- mostrar (S ps) = do
--     let aux = V.map (\p -> putStrLn ("a0="P.++show (eFicticia p) P.++ ", ws="P.++ show (pesos p))) ps
--     P.sequence_ aux
--     return ()
-- mostrar (E ps r) = do
--     let aux = V.map (\p -> putStrLn ("a0="P.++show (eFicticia p) P.++ ", ws="P.++ show (pesos p))) ps
--     P.sequence_ aux
--     putStrLn "--------------"
--     res <- mostrar r
--     return res    
-- mostrar (O ps r) = do
--     let aux = V.map (\p -> putStrLn ("a0="P.++show (eFicticia p) P.++ ", ws="P.++ show (pesos p))) ps
--     P.sequence_ aux
--     putStrLn "--------------"
--     res <- mostrar r
--     return res

-- mostrar' :: Red -> [[[Double]]]
-- mostrar' (S v ) = [[ mPer (v V.! x) |  x <- [0..((V.length v)-1)] ]]
-- mostrar' (O v r) = [[ mPer (v V.! x) |  x <- [0..((V.length v)-1)] ]]P.++(mostrar' r)

mPer :: Perceptron -> [Double]
mPer p = [eFicticia p]P.++(pesos p)


predecir :: Red -> [Double] -> [Double]
predecir (S v) ls = salida
    where
        salida = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]

predecir (O v r) ls = predecir r salida
    where
        salida = [ calculaSalida i |  i <- [0..n_pesos-1]]
        n_pesos = (P.length (pesos (v V.! 0))) - 1  
        ls' = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls' $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]

predecir (E v r) ls = predecir r salida
    where
        salida = [ calculaSalida i |  i <- [0..n_pesos-1]]
        n_pesos = P.length $ pesos (v V.! 0)
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]
 

salidaPerceptron :: Perceptron -> [Double] -> Double
salidaPerceptron (P a ws f _ ) ls = f $ P.sum $ P.map (\(a,b) -> a*b) (P.zip ws (a:ls))

propHaciaDelante :: Red -> [Double] -> [[(Perceptron, Double)]]
propHaciaDelante (S v) ls = [resultado]
    where
        resultado = P.zip (V.toList v) ls
        
propHaciaDelante (O v r) ls = resultado : (propHaciaDelante r salida)
    where
        resultado = P.zip (V.toList v) ls
        salida = [ calculaSalida i |  i <- [0..n_pesos-1]]
        n_pesos = (P.length (pesos (v V.! 0))) - 1
        ls' = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls' $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]

propHaciaDelante (E v r) ls = resultado : (propHaciaDelante r salida)
    where
        resultado = P.zip (V.toList v) ls
        salida = [ calculaSalida i |  i <- [0..n_pesos-1]]
        n_pesos = P.length $ pesos (v V.! 0)
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]

-- mostrar'' delante = P.map (\ls -> P.map (snd) ls) delante

propHaciaAtrasAux :: [[(Perceptron, Double)]] -> [Double] -> Double -> ([Double], Red)
propHaciaAtrasAux [ls] ys lr = (variaciones, capa)
    where
        variaciones = P.map (\i -> derivada i * diferencia i) [0..(P.length ls)-1]
        derivada i = fDerivada p inp
            where
                p = fst (ls!!i)
                inp = snd (ls!!i) + (P.head (pesos p)) * (eFicticia p)
        diferencia i = ys!!i - (fActivacion p inp)
            where
                p = fst (ls!!i)
                inp = snd (ls!!i) + (P.head (pesos p)) * (eFicticia p)
        pesosAct = [P.head(pesos p) + lr*(eFicticia p)*(variaciones!!i) | i <- [0..(P.length ls)-1], let p = fst(ls!!i)]
        capa = S (V.fromList [p {pesos = (pesosAct!!i):(P.tail ws)} | i <- [0..(P.length ls)-1], let p = fst(ls!!i), let ws = pesos p])

propHaciaAtrasAux (l:ls) ys lr = (variaciones, capa)
    where
        variaciones = P.map (\i -> derivada i * sumatorio i) [0..(P.length l)-1]
        derivada i = fDerivada p inp
            where
                p = fst (l!!i)
                inp = snd (l!!i) + (P.head (pesos p)) * (eFicticia p)
        sumatorio i = P.sum $ P.map (\(a,b) -> a*b) (P.zip (P.tail (pesos p)) (variaciones'))
            where
                a = eFicticia p
                p = fst (l!!i)
        resultado = propHaciaAtrasAux ls ys lr
        variaciones' = fst resultado
        red' = snd resultado
        actualizarPesos i =
            (ws!!0 + lr*(es!!0)*(vs!!0)):(P.map (\j -> ws!!j + lr*(es!!1)*(vs!!j)) [1..(P.length ws)-1])
            where
                p = fst (l!!i) 
                ws = pesos p
                es = [eFicticia p, fActivacion p (snd (l!!i) + (P.head (pesos p)) * (eFicticia p))]
                vs = (variaciones!!i) : variaciones'
        pesosAct = [ actualizarPesos i | i <- [0..(P.length l)-1]]
        perceptrones' = V.fromList  [ p {pesos = pesosAct!!i} |  i <- [0..(P.length pesosAct)-1], let p = fst(l!!i)]
        capa = O perceptrones' (snd resultado)

propHaciaAtras :: [[(Perceptron, Double)]] -> [Double] -> Double -> Red
propHaciaAtras (l:ls) ys lr = red
    where
        resultado = propHaciaAtrasAux ls ys lr
        variaciones' = fst resultado
        red' = snd resultado
        red = E perceptrones' red'
        actualizarPesos i =
            (P.map (\j -> ws!!j + lr*(es)*(vs!!j)) [0..(P.length ws)-1])
            where
                p = fst (l!!i)
                ws = pesos p
                es = snd (l!!i)
                vs = variaciones'
        pesosAct = [ actualizarPesos i | i <- [0..(P.length l)-1]]
        perceptrones' = V.fromList  [ p {pesos = pesosAct!!i} |  i <- [0..(P.length pesosAct)-1], let p = fst(l!!i)]

retropropagacion :: Red -> [([Double], [Double])] -> Int -> Double -> Red
retropropagacion red entrenamiento 0 lr = red
retropropagacion red entrenamiento epochs lr = retropropagacion red' entrenamiento (epochs-1) lr
    where
        red' = retropropagacionAux red entrenamiento lr

retropropagacionAux :: Red -> [([Double], [Double])] -> Double -> Red
retropropagacionAux red [] lr = red
retropropagacionAux red (e:entrenamiento) lr = retropropagacionAux red' entrenamiento lr
    where
        propDelante = propHaciaDelante red (fst e)
        red' = propHaciaAtras propDelante (snd e) lr


-- cuenta_bien :: Red -> Int -> Double ->[([Double], [Double])] -> Int 
-- cuenta_bien r e f ls = P.sum [ if (round (P.head (predecir ret x)))==round(P.head l) then 1 else 0 | (x,l) <- ls]
--     where
--         ret = retropropagacion r ls e f

{--
        Para la superación del trabajo, el código debe ser programación funcional y contener
como mínimo, de forma natural:

 
2 funciones básicas de prelude y Data.List, -- COMPLETADO
2 funciones recursivas -- COMPLETADO
2 funciones por patrones -- COMPLETADO
2 usos de guardas  -- COMPLETADO
2 usos de case of -- PENDIENTE (0/2)
2 usos de listas por comprensión -- COMPLETADO
2 usos de orden superior -- PENDIENTE (2/2)
declaraciones de tipos para todas las funciones definida -- COMPLETADO
2 usos de evaluación perezosa -- PENDIENTE (0/2)
Creación de un módulo -- COMPLETADO
2 tipos de datos nuevos y usos de éstos. -- COMPLETADO
2 tipos de datos abstractos o librerías vistos en la asignatura (por 
ejemplo, pilas, colas, map, matrix, array). -- PENDIENTE (1/2)
--}