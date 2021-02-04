-- Proyecto PD.
-- Búsqueda en Escalada con reinicio
-- Enfriamiento Simulado
-- Algoritmo Genético
-- PSO
-- ARCHIVO: IA
module IA where
    
import Prelude as P
import System.Random
import System.IO.Unsafe ( unsafePerformIO )  -- be careful! 
import Data.Vector as V
import Data.Matrix as M

--TIPOS
type Ind a = [a]
type Val = Double
--type Pr a = (Solucion a, Solucion a)
type Gi a = (IO (Ind a))
type Gs a = (Ind a -> IO (Ind a))
type Fv a = (Ind a -> Val)
type Me = (Val -> Val -> Bool)

data Solucion a = Solucion (Ind a) Val

instance (Show a) => Show (Solucion a) where
    show (Solucion i v) = "Solución: "P.++(show i)P.++"\nValoración: "P.++(show v)
    
data Problema a = Problema (Gi a) (Gs a) (Fv a) Me

prob :: IO Double
prob =  do
            xs <- (randomRIO (1, 10000))
            return (xs/10000)

removeIO :: IO a -> a
removeIO s = unsafePerformIO s

{--
b_escalada:
            Recibe:
                    Función Genera Estado Inicial
                        - Recibe : una tupla vacia para que se pueda producir la aleatoriedad.
                        - Devuelve: un estado inicial.
                    Función Genera Sucesor
                        - Recibe: estado.
                        - Devuelve: estado.
                    Función Valoración
                        - Recibe: estado
                        - Devuelve: valoración de ese estado.
                    Función Mejor
                        - Recibe: dos valoraciones.
                        - Devuelve: Bool.
            Devuelve: ( [ estado ] , valoración del estado )
--}

{- lista :: Int -> IO [Int]
lista n = lista_aux n []
lista_aux :: Int -> [Int] -> IO [Int]
lista_aux n ls = do
    if n == 0
        then
            return ls
        else do
            x <- aleatorio 1 9
            lista_aux (n-1) (x:ls) -}
  
bEscalada :: Problema a -> IO (Solucion a)
bEscalada p@(Problema gi gs fv me) = do
    i <- gi
    s <- gs i
    let sucesor = Solucion s (fv s)
    let actual = Solucion i (fv i)
    bEscaladaAux p actual sucesor

{--
b_escalada_aux:
            Recibe:
                    Actual: estado
                    Vecino: estado
                    Función Genera Sucesor
                        - Recibe: estado.
                        - Devuelve: estado.
                    Función Valoración
                        - Recibe: estado
                        - Devuelve: valoración de ese estado.
                    Función Mejor
                        - Recibe: dos valoraciones.
                        - Devuelve: Bool.
            
            Devuelve: ( [ estado ] , valoración del estado )
--}

bEscaladaAux :: Problema a -> Solucion a -> Solucion a -> IO (Solucion a)
bEscaladaAux p@(Problema _ gs fv me) actual@(Solucion iAct vAct) sucesor@(Solucion iSuc vSuc) = do
    iSucesor' <- gs $ iSuc
    let sucesor' = Solucion iSucesor' (fv iSucesor')
    if me (vSuc) (vAct)
        then
            bEscaladaAux p sucesor sucesor'
        else
            return actual
        

{--
b_escalada_reinicio:
            Recibe:
                    Función Genera Estado Inicial
                        - Recibe : una tupla vacia para que se pueda producir la aleatoriedad.
                        - Devuelve: un estado inicial.
                    Función Genera Sucesor
                        - Recibe: estado.
                        - Devuelve: estado.
                    Función Valoración
                        - Recibe: estado
                        - Devuelve: valoración de ese estado.
                    Función Mejor
                        - Recibe: dos valoraciones.
                        - Devuelve: Bool.
                    Número Reinicios: Número    
            
            Devuelve: ( [ estado ] , valoración del estado )
--}

bEscaladaReinicio :: Problema a -> Int -> IO (Solucion a)
bEscaladaReinicio p@(Problema _ gs fv me) nReinicios = do
    if nReinicios <= 0
        then
            error "El número de reinicios debe ser mayor o igual a 1."
        else do
            actual <- bEscalada p    
            bEscaladaReinicioAux p (nReinicios-1) actual



{--
b_escalada_reinicio_aux:
            Recibe:
                    Función Genera Estado Inicial
                        - Recibe : una tupla vacia para que se pueda producir la aleatoriedad.
                        - Devuelve: un estado inicial.
                    Función Genera Sucesor
                        - Recibe: estado.
                        - Devuelve: estado.
                    Función Valoración
                        - Recibe: estado
                        - Devuelve: valoración de ese estado.
                    Función Mejor
                        - Recibe: dos valoraciones.
                        - Devuelve: Bool.
                    Número Reinicios: Número   
                    Sol:  ( [ mejor estado actual ] , valoración del estado )
            
            Devuelve: ( [ estado ] , valoración del estado )
--}

bEscaladaReinicioAux :: Problema a -> Int -> Solucion a -> IO (Solucion a)
bEscaladaReinicioAux _ 0 sol = return sol
bEscaladaReinicioAux p@(Problema _ gs fv me) nReinicios actual@(Solucion iAct vAct) = do
    sucesor@(Solucion iSuc vSuc) <- bEscalada p    
    if me (vSuc) (vAct)
        then 
            bEscaladaReinicioAux p (nReinicios-1) sucesor
        else
            bEscaladaReinicioAux p (nReinicios-1) actual

eSimulado :: Problema a -> Double -> Double -> Int -> Int -> IO (Solucion a)
eSimulado p@(Problema gi gs fv me) t d ne ni = do
    if ne <= 0
        then
            error "El número de enfriamientos debe ser mayor que cero."
        else
            if ni <= 0
                then
                    error "El número de iteraciones debe ser mayor que cero."
                else do
                    iAct <- gi
                    let actual = (Solucion iAct (fv iAct))
                    eSimuladoEnfr p t d ne ni actual actual

eSimuladoEnfr :: Problema a -> Double -> Double -> Int -> Int -> Solucion a -> Solucion a -> IO (Solucion a)
eSimuladoEnfr _ _ _ 0 _ _ mejor= return mejor
eSimuladoEnfr p t d ne ni actual mejor = do
    let t' = t * d
    let ne' = ne-1
    (actual',mejor') <- eSimuladoIter p t' ni actual mejor
    eSimuladoEnfr p t' d ne' ni actual' mejor'

eSimuladoIter :: Problema a -> Double -> Int -> Solucion a-> Solucion a -> IO (Solucion a, Solucion a)
eSimuladoIter _ _ 0 actual mejor = return (actual, mejor)
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
        
sorteo :: Double -> Double -> Double -> IO Bool             
sorteo vc va t = do
    let p = exp(-abs( (vc-va) / t))
    r <- prob
    return (r <= p)

relu :: Double -> Double
relu = max 0

umbral :: Double -> Double
umbral x
    |  x > 0 = 1
    |  x <= 0 = 0

sigmoide :: Double -> Double
sigmoide x = 1 / (1+exp(-x))

dSigmoide :: Double -> Double
dSigmoide x = sigmoide x / (1-sigmoide x)

dRelu :: Double -> Double
dRelu x = umbral x

data Perceptron = P {   
                        eFicticia :: Double,
                        pesos :: [Double],
                        fActivacion :: (Double -> Double),
                        fDerivada :: (Double -> Double)
                    }

data Red    = E (Vector Perceptron) Red
            | O (Vector Perceptron) Red
            | S (Vector Perceptron)

vector :: Vector Int
vector = V.fromList [5]

-- Número de perceptrones por capa, Pesos de cada perceptron, Funciones de cada capa
-- 1 -1 [4,3,2,1] [Funciones] [Derivadas] -> Red 
crear :: Double -> Double -> [Int] -> [Double -> Double] -> [Double -> Double] -> Red
crear w a (c:cs) fs ds = E (V.fromList [ P 0 [ w | _ <- [1..p_sup]] id id | _ <- [1..c]]) (crear' w a cs fs ds)
    where 
        p_sup = P.head cs

crear' :: Double -> Double -> [Int] -> [Double -> Double] -> [Double -> Double] -> Red
crear' w a [c] (f:fs) (d:ds) = S (V.fromList [ P a [ w | _ <- [1..c-1]] f d | _ <- [1..c]])
crear' w a (c:cs) (f:fs) (d:ds) = O (V.fromList [ (P a [ w |  _ <- [0..p_sup]] f d) | _ <- [1..c] ]) (crear' w a cs fs ds)
    where 
        p_sup = P.head cs

-- crear w a e [c] (f:fs) (d:ds) = S (V.fromList [ P a [w] f d | _ <- [1..c]])
-- crear w a e (c:cs) (f:fs) (d:ds) = O (V.fromList [ (P a [ w |  _ <- [0..P.head cs]] f d) | x <- [1..c] ]) (crear w a c cs fs ds)
-- crear w a e cs fs ds = 
-- crear w a e cs fs ds =    
-- crear' w a e [c] (f:fs) (d:ds) = S (V.fromList [ P a [ w | _ <- [1..e+1]] f d | _ <- [1..c]])
-- crear' w a e (c:cs) (f:fs) (d:ds) = O (V.fromList [ (P a [ w |  _ <- [1..e+1]] f d) | x <- [1..c] ]) (crear w a c cs fs ds)

-- crear 1.0 -1 2 [2,2,2] [sigmoide, sigmoide, sigmoide]

-- 1,1 y sale 1,1

mostrar :: Red -> IO()
mostrar (S ps) = do
    let aux = V.map (\p -> putStrLn ("a0="P.++show (eFicticia p) P.++ ", ws="P.++ show (pesos p))) ps
    P.sequence_ aux
    return ()
mostrar (E ps r) = do
    let aux = V.map (\p -> putStrLn ("a0="P.++show (eFicticia p) P.++ ", ws="P.++ show (pesos p))) ps
    P.sequence_ aux
    putStrLn "--------------"
    res <- mostrar r
    return res    
mostrar (O ps r) = do
    let aux = V.map (\p -> putStrLn ("a0="P.++show (eFicticia p) P.++ ", ws="P.++ show (pesos p))) ps
    P.sequence_ aux
    putStrLn "--------------"
    res <- mostrar r
    return res

mostrar' :: Red -> [[[Double]]]
mostrar' (S v ) = [[ mPer (v V.! x) |  x <- [0..((V.length v)-1)] ]]
mostrar' (O v r) = [[ mPer (v V.! x) |  x <- [0..((V.length v)-1)] ]]P.++(mostrar' r)

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
        salida = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
        ls' = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
        resultado = [ (p, input p) |  x <- [0..((V.length v)-1)], let p = (v V.! x)] 
        input p = P.sum $ P.map (\(a,b) -> a*b) (P.zip (pesos p) ((eFicticia p):ls))
        
propHaciaDelante (O v r) ls = resultado : (propHaciaDelante r salida)
    where
        resultado = P.zip (V.toList v) ls
        salida = [ calculaSalida i |  i <- [0..n_pesos-1]]
        n_pesos = (P.length (pesos (v V.! 0))) - 1  -- NOS HEMOS QUEDADO POR AQUÍ
        --ls' = [ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
        ls' = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls' $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]

propHaciaDelante (E v r) ls = resultado : (propHaciaDelante r salida)
    where
        resultado = P.zip (V.toList v) ls
        salida = [ calculaSalida i |  i <- [0..n_pesos-1]]
        n_pesos = P.length $ pesos (v V.! 0)
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ P.zip ls $ [ (pesos p)!!i | j <- [0..(V.length v)-1], let p = (v V.! j)]

propHaciaAtras :: [[(Perceptron, Double)]] -> [Double] -> Double -> ([Double], Red)
propHaciaAtras [ls] ys lr = (variaciones, capa)
    where
        variaciones = P.map (\i -> derivada i * diferencia i) [0..(P.length ls)-1]
        derivada i = fDerivada p inp
            where
                p = fst (ls!!i)
                inp = snd (ls!!i)
        diferencia i = ys!!i - (fActivacion p inp)
            where
                p = fst (ls!!i)
                inp = snd (ls!!i)
        pesosAct = [P.head(pesos p) + lr*(eFicticia p)*(variaciones!!i) | i <- [0..(P.length ls)-1], let p = fst(ls!!i)]
        capa = S (V.fromList [p {pesos = (pesosAct!!i):(P.tail ws)} | i <- [0..(P.length ls)-1], let p = fst(ls!!i), let ws = pesos p])

propHaciaAtras (l:ls) ys lr = (variaciones, capa)
    where
        variaciones = P.map (\i -> derivada i * sumatorio i) [0..(P.length l)-1]
        derivada i = fDerivada p inp
            where
                p = fst (l!!i)
                inp = snd (l!!i)
        sumatorio i = P.sum $ P.map (\(a,b) -> a*b) (P.zip (pesos p) (a:variaciones'))
            where
                a = eFicticia p
                p = fst (l!!i)
        resultado = propHaciaAtras ls ys lr
        variaciones' = fst resultado
        red' = snd resultado
        actualizarPesos i = -- NOS HEMOS QUEDADO POR AQUÍ
            (ws!!0 + lr*(es!!0)*(vs!!0)):(P.map (\j -> ws!!j + lr*(es!!1)*(vs!!j)) [1..(P.length ws)-1])
            where
                p = fst (l!!i) 
                ws = pesos p
                es = [eFicticia p, fActivacion p (snd (l!!i))]
                vs = (variaciones!!i) : variaciones'


        -- pesosAct = [ [ ws!!e + lr* | e <- [0..((P.length ws)-1)]] | i <- [0..(P.length l)-1], let p = fst (l!!i), let ws = pesos p]
        pesosAct = [ actualizarPesos i | i <- [0..(P.length l)-1]]
        -- c = V.fromList [p {pesos = (pesosAct!!i):(P.tail ws)}]
        perceptrones' = V.fromList  [ p {pesos = pesosAct!!i} |  i <- [0..(P.length pesosAct)-1], let p = fst(l!!i)]
        --perceptrones' = V.fromList  [ p {pesos = [0,0,0,0]} |  i <- [0..(P.length pesosAct)-1], let p = fst(l!!i)]
        capa = O perceptrones' (snd resultado)

--      C1      C2
--       |
--   -  n1      |
--   -  n2      n4
--   -  n3  {(p1,p2) = 23412, (p2,p3 = agsfg)}


  {-
a0=1.0, ws=[1.0,1.0]
a0=1.0, ws=[1.0,1.0]
a0=1.0, ws=[1.0,1.0]
--------------
a0=1.0, ws=[1.0]
  -}

-- p1   aa p2    map(p1,p2) 
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
        red' = snd $ propHaciaAtras propDelante (snd e) lr

{--

Entrada  Salida
1 1        1
0 0        0
1 0        0
0 1        0

[([1,1],[1]), ([0,0],[0]), ([1,0],[0]), ([0,1],[0])]

[([1,1],[1]), ([0,0],[0]), ([1,0],[1]), ([0,1],[1])]

[([1,1],[0]), ([0,0],[0]), ([1,0],[1]), ([0,1],[1])]



--}
{-
1) Inicializar los pesos de la red (aleatoriamente, usualmente
con valores cercanos a cero, positivos o negativos)
2) Repetir hasta que se satisfaga el criterio de parada
    1) Para cada ejemplo (~ x , ~ y ) ∈ D hacer:
        1) Calcular la salida a i de cada unidad i, propagando
        valores hacia adelante
        2) Calcular los errores ∆ i de cada unidad i y actualizar
        los pesos w ji , propagando valores hacia detrás
3) Devolver red
-}
--[w 5,6 ∆ 6 + w 5,7 ∆ 7 ]

--w 0,7 ←− w 0,7 + ηa 0 ∆ 7

-- [[(p,12312),(p,12312)],[(p,12312)]] []

-- retropropagacion (O v r) [o:os]

-- g(w0*a0 + w1*a1...)
-- w0*a0 + w1*a1... = in

-- g'(in)
{-
1) Para cada unidad i en la capa de salida hacer
    ∆ i ← g l 0 (in i )(y i − a i )
2) Para l desde L − 1 hasta 1 (decrementando l) hacer
    1) Para cada nodo j en la capa l hacer
        1) ∆ j ← g l 0 (in j ) i w ji ∆ i (donde el sumatorio anterior tiene un sumando por cada unidad i de la capa l + 1)
        2) Para cada nodo i en la capa l + 1 hacer w ji ← w ji + ηa j ∆ i
-}
-- 1) Inicializar los pesos de la red (aleatoriamente, usualmente con valores cercanos a cero, positivos o negativos)
-- 2) Repetir hasta que se satisfaga el criterio de parada
-- 1) Para cada ejemplo (~x, ~y) ∈ D hacer:
-- 1) Calcular la salida ai de cada unidad i, propagando
-- valores hacia adelante
-- 2) Calcular los errores ∆i de cada unidad i y actualizar
-- los pesos wji, propagando valores hacia detr´as
-- 3) Devolver red

-- [0.5 0.5 0.5 0.5]

--[1,2,3]
-- aplicar:: [Int] -> Matrix Perceptron -> [Int]


{--
// Bias, Pesos, Función

[3,2,1]

[sigmoide, sigmoide, relu]


funcion  [1,2,3,4]

    [Perceptron, Perceptron, Perceptron]
    [Perceptron, Perceptron, Perceptron]
    [Perceptron, Perceptron]

    [relu,
     relu,
     sigmoide]

relu :: Int -> Int
relu x = max(0, x)

--}


{--
        Para la superación del trabajo, el código debe ser programación funcional y contener
como mínimo, de forma natural:

 
2 funciones básicas de prelude y Data.List, -- COMPLETADO
2 funciones recursivas -- COMPLETADO
2 funciones por patrones -- COMPLETADO
2 usos de guardas  -- COMPLETADO
2 usos de case of -- PENDIENTE (0/2)
2 usos de listas por comprensión -- COMPLETADO
2 usos de orden superior -- PENDIENTE (1/2)
declaraciones de tipos para todas las funciones definida -- COMPLETADO
2 usos de evaluación perezosa -- PENDIENTE (0/2)
Creación de un módulo -- COMPLETADO
2 tipos de datos nuevos y usos de éstos. -- COMPLETADO
2 tipos de datos abstractos o librerías vistos en la asignatura (por 
ejemplo, pilas, colas, map, matrix, array). -- PENDIENTE (0/2)



--}