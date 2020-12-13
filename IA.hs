-- Proyecto PD.
-- Búsqueda en Escalada con reinicio
-- Enfriamiento Simulado
-- Algoritmo Genético
-- PSO
-- ARCHIVO: IA
module IA where
    
import Prelude
import System.Random
import System.IO.Unsafe  -- be careful! 

--TIPOS
type Ind a = [a]
type Val = Double

type Gi a = (() -> Ind a)
type Gs a = (Ind a -> Ind a)
type Fv a = (Ind a -> Val)
type Me = (Val -> Val -> Bool)

type Solucion a = (Ind a, Val)
-- type Problema_enfriamiento = (gi, gs, fv, m, t, d, ne, ni)  

aleatorio :: (Random a, Num a) => a -> a -> a
aleatorio i j = unsafePerformIO (randomRIO (i,j))

prob :: (Random a, Enum a, Fractional a) => a
prob = [0.0001,0.0002..1]!!(unsafePerformIO (randomRIO (0,9999)))    

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
  
b_escalada :: Gi a -> Gs a -> Fv a -> Me -> Solucion a
b_escalada gi gs fv me = 
    b_escalada_aux actual sucesor gs fv me
    where
        i = gi()
        s = gs i
        sucesor = (s, fv s)
        actual = (i, fv i)

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

b_escalada_aux :: Solucion a -> Solucion a -> Gs a -> Fv a -> Me -> Solucion a
b_escalada_aux actual vecino gs fv me
    | me (snd vecino) (snd actual) = b_escalada_aux vecino vecino' gs fv me
    | otherwise = actual
    where
        sucesor = gs $ fst vecino
        vecino' = (sucesor, fv sucesor)

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

b_escalada_reinicio :: Gi a -> Gs a -> Fv a -> Me -> Int -> Solucion a
b_escalada_reinicio gi gs fv me n_reinicios
    | n_reinicios <= 0  = error "El número de reinicios debe ser mayor o igual a 1."
    | otherwise         = b_escalada_reinicio_aux gi gs fv me (n_reinicios-1) sol
    where
        sol = b_escalada gi gs fv me

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

b_escalada_reinicio_aux :: Gi a -> Gs a -> Fv a -> Me -> Int -> Solucion a -> Solucion a
b_escalada_reinicio_aux _ _ _ _ 0 sol = sol
b_escalada_reinicio_aux gi gs fv me n_reinicios sol
    | me (snd c) (snd sol)   = b_escalada_reinicio_aux gi gs fv me (n_reinicios-1) c
    | otherwise                 = b_escalada_reinicio_aux gi gs fv me (n_reinicios-1) sol
    where
        c = b_escalada gi gs fv me


--type Par a = (a,a)

--type Sol a = ([a], Double)

--type Fv a = ([a] -> Double)
--type Gi a = (() -> [a])
--type Gs a = ([a] -> [a])
--type M = (Double, Double, Bool)
--type P_optimo a = ((() -> [a]), ([a] -> [a]), ([a] -> Double), (Double, Double, Bool))


e_simulado :: Gi a -> Gs a -> Fv a -> Me -> Double -> Double -> Int -> Int -> Solucion a
e_simulado gi gs fv me t d ne ni
    | ne <= 0 = error "El número de enfriamientos debe ser mayor que cero."
    | ni <= 0 = error "El número de iteraciones debe ser mayor que cero."
    | otherwise = e_simulado_enfr gs fv me t d ne ni actual' actual'
    where
        actual = gi()
        v_actual = fv actual
        actual' = (actual, v_actual)

e_simulado_enfr :: Gs a -> Fv a -> Me -> Double -> Double -> Int -> Int -> Solucion a -> Solucion a -> Solucion a
e_simulado_enfr _ _ _ _ _ 0 _ _ mejor = mejor
e_simulado_enfr gs fv me t d ne ni actual mejor = 
    e_simulado_enfr gs fv me t' d ne' ni (fst nueva_iter) (snd nueva_iter)
    where
        t' = t * d
        ne' = ne-1
        nueva_iter = e_simulado_iter gs fv me t' d ni actual mejor

e_simulado_iter :: Gs a -> Fv a -> Me -> Double -> Double -> Int -> Solucion a-> Solucion a -> (Solucion a, Solucion a)
e_simulado_iter _ _ _ _ _ 0 actual mejor = (actual, mejor) 
e_simulado_iter gs fv me t d ni actual mejor
    | aceptar_candidata = e_simulado_iter gs fv me t d l candidata n_mejor
    | otherwise = e_simulado_iter gs fv me t d l actual mejor
    where 
        sucesor = gs $ fst actual
        candidata = (sucesor, fv sucesor)
        incremento = snd candidata - snd actual
        n_mejor = candidata
        --n_mejor = if me (snd candidata) (snd mejor) then candidata else mejor
        l = ni - 1
        aceptar_candidata = True
        --aceptar_candidata = incremento < 0 || sorteo (snd candidata) (snd actual) t 

{--
def iniciar(self):
        t = self.t_inicial
        factor_descenso = self.factor_descenso
        actual = self.genera_inicial()
        valor_actual = self.f_valoracion(actual)
        mejor  = actual
        valor_mejor = valor_actual

        for _ in range(self.n_enfriamientos):
            for _ in range(self.n_iteraciones):
                candidata = self.genera_sucesor(actual)
                valor_candidata = self.f_valoracion(candidata)

                if self.mejor(valor_candidata, valor_actual) or self.sorteo(valor_candidata, valor_actual, t):
                    actual = candidata
                    valor_actual = valor_candidata

                if self.mejor(valor_actual, valor_mejor):
                    mejor = actual
                    valor_mejor = valor_actual

            t *= factor_descenso

        return (mejor, valor_mejor)
--}

sorteo :: Double -> Double -> Double -> Bool             
sorteo vc va t
    | r <= p = True
    | otherwise = False
    where
        p = exp(-abs( (vc-va) / t)) :: Double
        r = prob :: Double

{--
FUNCION ENFRIAMIENTO-SIMULADO(T-INICIAL,FACTOR-DESCENSO,
N-ENFRIAMIENTOS,N-ITERACIONES)
1. Crear las siguientes variables locales:
    1.1 TEMPERATURA (para almacenar la temperatura actual),
    inicialmente con valor T-INICIAL.
    1.2 ACTUAL (para almacenar el estado actual), cuyo valor
    inicial es GENERA-ESTADO-INICIAL().
    1.3 VALOR-ACTUAL igual a F-VALORACIÓN(ACTUAL)
    1.4 MEJOR (para almacenar el mejor estado
    encontrado hasta el momento), inicialmente ACTUAL.
    1.5 VALOR-MEJOR (para almacenar el valor de MEJOR),
    inicialmente igual a VALOR-ACTUAL
2. Iterar un número de veces igual a N-ENFRIAMIENTOS:
    2.1 Iterar un número de veces igual a N-ITERACIONES:
        2.1.1 Crear las siguientes variables locales:
            2.1.1.1 CANDIDATA, una solución vecina de ACTUAL,
                    generada por GENERA-SUCESOR.
            2.1.1.2 VALOR-CANDIDATA, el valor de CANDIDATA.
            2.1.1.3 INCREMENTO, la diferencia entre VALOR-CANDIDATA y
                    VALOR-ACTUAL
        2.1.2 Cuando INCREMENTO es negativo, o se acepta
            probabilísticamente la solución candidata,
            hacer ACTUAL igual a VECINA
            y VALOR-ACTUAL igual a VALOR-VECINA.
        2.1.3 Si VALOR-ACTUAL es mejor que VALOR-MEJOR,
        actualizar MEJOR con ACTUAL
        y VALOR-MEJOR con VALOR-ACTUAL.
    2.2 Disminuir TEMPERATURA usando FACTOR-DESCENSO
3. Devolver MEJOR y VALOR-MEJOR

import random
import math

class Enfriamiento_simulado():
    def __init__(self, t_inicial, factor_descenso, n_enfriamientos, n_iteraciones):
        self.t_inicial = t_inicial
        self.factor_descenso = factor_descenso
        self.n_enfriamientos = n_enfriamientos
        self.n_iteraciones = n_iteraciones

    def mejor(self, valor_candidata, valor_actual):
        pass

    def f_valoracion(self, estado):
        pass

    def genera_sucesor(self, estado):
        pass

    def genera_inicial(self):
        pass

    def sorteo(self, valor_candidata, valor_actual, t):
        p = math.exp(-abs((valor_candidata-valor_actual)/t))
        r = random.random()

        if(r <= p):
            aceptar = True
        else:
            aceptar = False

        return aceptar

    def iniciar(self):
        t = self.t_inicial
        factor_descenso = self.factor_descenso
        actual = self.genera_inicial()
        valor_actual = self.f_valoracion(actual)
        mejor  = actual
        valor_mejor = valor_actual

        for _ in range(self.n_enfriamientos):
            for _ in range(self.n_iteraciones):
                candidata = self.genera_sucesor(actual)
                valor_candidata = self.f_valoracion(candidata)

                if self.mejor(valor_candidata, valor_actual) or self.sorteo(valor_candidata, valor_actual, t):
                    actual = candidata
                    valor_actual = valor_candidata

                if self.mejor(valor_actual, valor_mejor):
                    mejor = actual
                    valor_mejor = valor_actual

            t *= factor_descenso

        return (mejor, valor_mejor)

    def enfriamiento_simulado(problema, t_inicial, factor_descenso,
                           n_enfriamientos, n_iteraciones):  

            actual=problema.genera_estado_inicial()
            valor_actual=problema.valoracion(actual)
            mejor=actual
            valor_mejor=valor_actual
            T=t_inicial
            for _ in range(n_enfriamientos):
                for _ in range(n_iteraciones):
                    candidata = problema.genera_sucesor(actual)
                    valor_candidata = problema.valoracion(candidata)
                    if aceptar_e_s(valor_candidata, valor_actual, T, problema.mejor):
                        actual=candidata
                        valor_actual=valor_candidata
                        if problema.mejor(valor_actual, valor_mejor):
                            mejor = actual
                            valor_mejor = valor_actual
                T *= factor_descenso
            return (mejor,valor_mejor)    
--}
