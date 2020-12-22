-- Proyecto PD.
-- Búsqueda en Escalada con reinicio
-- Enfriamiento Simulado
-- Algoritmo Genético
-- PSO
-- ARCHIVO: IA
module IA where
    
import Prelude
import System.Random
import System.IO.Unsafe ( unsafePerformIO )  -- be careful! 

--TIPOS
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

-- type Problema_enfriamiento = (gi, gs, fv, m, t, d, ne, ni)  

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
  
b_escalada :: Problema a -> IO (Solucion a)
b_escalada p@(Problema gi gs fv me) = do
    i <- gi
    s <- gs i
    let sucesor = Solucion s (fv s)
    let actual = Solucion i (fv i)
    b_escalada_aux p actual sucesor

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

b_escalada_aux :: Problema a -> Solucion a -> Solucion a -> IO (Solucion a)
b_escalada_aux p@(Problema _ gs fv me) actual@(Solucion i_act v_act) sucesor@(Solucion i_suc v_suc) = do
    i_sucesor' <- gs $ i_suc
    let sucesor' = Solucion i_sucesor' (fv i_sucesor')
    if me (v_suc) (v_act)
        then
            b_escalada_aux p sucesor sucesor'
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

b_escalada_reinicio :: Problema a -> Int -> IO (Solucion a)
b_escalada_reinicio p@(Problema _ gs fv me) n_reinicios = do
    if n_reinicios <= 0
        then
            error "El número de reinicios debe ser mayor o igual a 1."
        else do
            actual <- b_escalada p    
            b_escalada_reinicio_aux p (n_reinicios-1) actual

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

b_escalada_reinicio_aux :: Problema a -> Int -> Solucion a -> IO (Solucion a)
b_escalada_reinicio_aux _ 0 sol = return sol
b_escalada_reinicio_aux p@(Problema _ gs fv me) n_reinicios actual@(Solucion i_act v_act) = do
    sucesor@(Solucion i_suc v_suc) <- b_escalada p    
    if me (v_suc) (v_act)
        then 
            b_escalada_reinicio_aux p (n_reinicios-1) sucesor
        else
            b_escalada_reinicio_aux p (n_reinicios-1) actual

e_simulado :: Problema a -> Double -> Double -> Int -> Int -> IO (Solucion a)
e_simulado p@(Problema gi gs fv me) t d ne ni = do
    if ne <= 0
        then
            error "El número de enfriamientos debe ser mayor que cero."
        else
            if ni <= 0
                then
                    error "El número de iteraciones debe ser mayor que cero."
                else do
                    i_act <- gi
                    let actual = (Solucion i_act (fv i_act))
                    e_simulado_enfr p t d ne ni actual actual

e_simulado_enfr :: Problema a -> Double -> Double -> Int -> Int -> Solucion a -> Solucion a -> IO (Solucion a)
e_simulado_enfr _ _ _ 0 _ _ mejor= return mejor
e_simulado_enfr p t d ne ni actual mejor = do
    let t' = t * d
    let ne' = ne-1
    (actual',mejor') <- e_simulado_iter p t' ni actual mejor
    e_simulado_enfr p t' d ne' ni actual' mejor'

e_simulado_iter :: Problema a -> Double -> Int -> Solucion a-> Solucion a -> IO (Solucion a, Solucion a)
e_simulado_iter _ _ 0 actual mejor = return (actual, mejor)
e_simulado_iter p@(Problema _ gs fv me) t ni actual@(Solucion i_act v_act) mejor@(Solucion i_mej v_mej) = do
    i_suc <- gs i_act
    let sucesor@(Solucion i_suc' v_suc') = Solucion i_suc (fv i_suc)
    let incremento = v_suc' - v_act
    let mejor' = if me v_suc' v_mej then sucesor else mejor
    let ni' = ni - 1
    gana_sorteo <- sorteo v_suc' v_act t
    let aceptar_candidata = incremento < 0 || gana_sorteo
    if aceptar_candidata
        then
            e_simulado_iter p t ni' sucesor mejor'
        else
            e_simulado_iter p t ni' actual mejor'
        
sorteo :: Double -> Double -> Double -> IO Bool             
sorteo vc va t = do
    let p = exp(-abs( (vc-va) / t))
    r <- prob
    return (r <= p)

{--
        Para la superación del trabajo, el código debe ser programación funcional y contener
como mínimo, de forma natural:

Dos usos de cada concepto básico de programación funcional visto en la asignatura. Es
decir: al menos usar 
2 funciones básicas de prelude y Data.List, 
definir 2 funciones recursivas, 
definir 2 funciones por patrones, 
2 usos de guardas, 
2 usos de case of, 
2 usos de listas por comprensión, 
2 usos de orden superior, declaraciones de tipos para todas las funciones definidas, 
2 usos de evaluación perezosa, etc.

Creación de un módulo

Creación de dos tipos de datos nuevos y usos de éstos.

Uso de al menos dos de tipos de datos abstractos o librerías vistos en la asignatura (por
ejemplo, pilas, colas, map, matrix, array).
--}