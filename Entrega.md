# **Librería IA (Inteligencia Artificial) para Haskell**

## **Asignatura**
* **Programación Declarativa** - [Página web](http://www.cs.us.es/cursos/pd/Contents.html#home)
* **Tercero de Ingeniería Informática - Tecnologías Informáticas**
* **Universidad de Sevilla**


## **Autores**
* **Andrés Carrasco Garzón** - *andcargar* - *andcargar@alumn.us.es*
* **Bruno González Llaga** - *brugonlla* - *brugonlla@alum.us.es*

## **Resumen**
* **Temática:** Inteligencia Artificial
* **Problema a resolver:** Implementación de una librería para resolver problemas de optimización

## **Índice**
* **Descripción** - Página 1
* **Estructura del código** - Página 1
* **Elementos mínimos exigidos** - Página 1
* **Ejemplos propuestos** - Página 1

## **Descripción**

El proyecto consiste en la implementación de una librería con algunos de los algoritmos y estructuras vistas en la asignatura de Inteligencia Artificial. De entre todo, hemos decidido implementar:

* [Algoritmos de búsqueda local](#algoritmos-de-busqueda-local) 
    - [Búsqueda en escalada](#busqueda-en-escalada)
    - [Búsqueda en escalada con reinicio](#busqueda-en-escalada)
    - [Enfriamiento simulado](#enfriamiento-simulado)
* [Redes neuronales](#redes-neuronales)
    - [Creación de la red](#busqueda-en-escalada)
    - [Predicción de valores](#busqueda-en-escalada)
    - [Algoritmo de retropropagación](#busqueda-en-escalada)


### **Algoritmos de búsqueda local**

#### **Búsqueda en escalada**

> El Algoritmo de Escalada Simple o Ascenso de colinas es una técnica de optimización matemática que pertenece a la familia de los algoritmos de búsqueda local. Es un algoritmo iterativo que comienza con una solución arbitraria a un problema, luego intenta encontrar una mejor solución variando incrementalmente un único elemento de la solución. Si el cambio produce una mejor solución, otro cambio incremental se le realiza a la nueva solución, repitiendo este proceso hasta que no se puedan encontrar mejoras.   **- Wikipedia**

El pseudocódigo usado es el siguiente:

1. Generamos una solución aleatoria, y calculamos su valoración según una función de valoración.
2. Generamos una nueva solución a partir de la solución actual, a la cual también se le calcula su valoración usando la misma función de valoración.
3. Comparamos la valoración de ambas soluciones según una función que defina cuál es mejor:
    - 3.1. Si la nueva solución es mejor que la actual, entonces sustituimos la actual por la nueva, y repetimos el algoritmo desde el paso 2.
    - 3.2. Si la nueva solución es peor que la actual, el algoritmo termina y devolvemos la solución actual.

#### **Búsqueda en escalada con reinicio**
> Hill climbing de reinicio aleatorio es meta-algoritmo construido sobre la base de hill climbing. Es también conocido como Shotgun hill climbing. Este realiza, iterativamente, el hill-climbing, cada vez con una condición inicial aleatoria *x<sub>0</sub>*. La mejor *x<sub>m</sub>* es guardada: si una nueva corrida del hill climbing produce una mejor *x<sub>m</sub>* que el estado guardado, lo reemplaza. **- Wikipedia**

El pseudocódigo usado es el siguiente:

1. Ejecutamos el algoritmo de búsqueda en escalada para generar la solución actual.
2. Calculamos la valoración de la solución actual mediante una función de valoración.
3. Iteramos un número de veces igual a N-REINICIOS:
    - 3.1. Generamos una nueva solución con el algoritmo de búsqueda en escalada.
    - 3.2. Calculamos la valoración de la nueva solución mediante una función de valoración.
    - 3.3. Comparamos la valoración de ambas soluciones mediante una función que defina cuál es mejor:
        - 3.3.1. Si la nueva solución es mejor que la actual, entonces sustituimos la actual por la nueva.
        - 3.3.2. Si la nueva solución es peor que la actual, entonces no cambiamos nada.
4. Devolvemos la solución actual.

<br>
<br>

#### **Enfriamiento simulado**
> Es un algoritmo de búsqueda metaheurística para problemas de optimización global; el objetivo general de este tipo de algoritmos es encontrar una buena aproximación al valor óptimo de una función en un espacio de búsqueda grande. A este valor óptimo se lo denomina "óptimo global". En cada iteración, el método de enfriamiento simulado evalúa algunos vecinos del estado actual *s* y probabilísticamente decide entre efectuar una transición a un nuevo estado *s'* o quedarse en el estado *s*. **- Wikipedia**

El pseudocódigo usado es el siguiente:

1. Creamos las siguientes variables locales.
    - 1.1. TEMPERATURA, con T-INICIAL.
    - 1.2. ACTUAL con una solución generada aleatoriamente.
    - 1.3. VALOR-ACTUAL con la valoración de ACTUAL mediante una función de valoración.
    - 1.4. MEJOR con ACTUAL
    - 1.5. VALOR-MEJOR con VALOR-ACTUAL

2. Iteramos un número de veces igual a N-ENFRIAMIENTOS:
    - 2.1. Iteramos un número de veces igual a N-ITERACIONES:
        - 2.1.1. Creamos las siguientes variables locales:
            - 2.1.1.1. CANDIDATA, generada a partir de ACTUAL con una función que genera sucesores.
            - 2.1.1.2. VALOR-CANDIDATA, con la valoración de CANDIDATA mediante una función de valoración.
            - 2.1.1.3. INCREMENTO, la diferencia entre VALOR-CANDIDATA y VALOR-ACTUAL.
        - 2.1.2. Cuando INCREMENTO es negativo, o se acepta probabilísticamente CANDIDATA, hacemos ACTUAL igual a CANDIDATA, y V-ACTUAL igual a VALOR-VECINA.
        - 2.1.3. Si VALOR-ACTUAL es mejor que VALOR-MEJOR, entonces hacemos MEJOR igual a ACTUAL.
    - 2.2. Hacemos TEMPERATURA igual a TEMPERATURA * FACTOR-DESCENSO.
3. Devolvemos MEJOR y VALOR-MEJOR


### **Redes neuronales**
#### **Creación de la red**
La creación de una red neuronal consiste en la unión de varias neuronas agrupadas en capas, que reciben el nombre de capas de entrada, si es la primera capa; capas ocultas, si son las capas intermedias; o capas de salida, si es la última capa.

Todas las neuronas de una misma capa tienen la misma función de activación, aunque ésta puede variar entre distintas capas. También, en esta librería sólo se contemplan las redes neuronales densas, en las que cada neurona de una capa están conectadas con todas las neuronas de la capa siguiente.

Cada neurona tiene una entrada ficticia, que en esta librería todas comparten el mismo valor, y también tienen unos pesos ficticios, que en esta librería se inicializan todas con el mismo valor (definido por el usuario).

Además, cada neurona tiene una función de activación y la derivada de ésta. Esta librería ofrece las funciones "relu" y "sigmoide", junto con sus derivadas "dRelu" y "dSigmoide", aunque el usuario puede definir las suyas propia.

#### **Predicción de valores**
La predicción de valores consiste en pasar unos valores de entrada por la capa de entrada de la red, y transmitir esos valores hacia la capa de salida, realizando en cada neurona los cálculos correspondientes. Los valores de la capa de salida se pueden interpretar de distintas maneras dependiendo del propósito de la red neuronal.

Por ejemplo, un único valor de salida podría interpretarse como una probabilidad de que ocurra algo o no, y varios valores de salida podrían interpretarse como las probabilidades de que una entrada corresponda a una clasificación u otra.

#### **Algoritmo de retropropagación**

> La propagación hacia atrás de errores o retropropagación (del inglés backpropagation) es un método de cálculo del gradiente utilizado en algoritmos de aprendizaje supervisado utilizados para entrenar redes neuronales artificiales. El método emplea un ciclo propagación – adaptación de dos fases. Una vez que se ha aplicado un patrón a la entrada de la red como estímulo, este se propaga desde la primera capa a través de las capas siguientes de la red, hasta generar una salida. La señal de salida se compara con la salida deseada y se calcula una señal de error para cada una de las salidas. **-Wikipedia**

De manera resumida, el algoritmo de retropropagación sirve para entrenar a una red neuronal, actualizando los pesos de cada capa para que la salida de cada entrada se acerque al valor esperado.

El pseudocódigo usado es el siguiente:

1. Inicializar los pesos de la red 
2. Repetir hasta que se satisfaga el criterio de parada
    - 2.1 Para cada ejemplo (~x, ~y) ∈ D hacer:
        - 2.1.1 Calcular la salida a<sub>i</sub> de cada unidad i, propagando valores hacia adelante.
        - 2.1.2 Calcular los errores ∆i de cada unidad i y actualizar los pesos w<sub>ji</sub>, propagando valores hacia detrás
3. Devolver red

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## **Estructura el código**
Nuestra libreria esta dividida en varias secciones:

* **Tipos usados en Búsqueda en Escalada y Enfriamiento Simulado** - *Línea 44* <br> Hemos creado una serie de sinónimos mediante *type* para referirnos a los distintos elementos del problema:
    - Individuo de una población. - **Ind**
    - Valoración de un individuo. - **Val**
    - Función de generación inicial de un individuo. - **Gi**
    - Función de generación de un sucesor a partir de un individuo. - **Gs**
    - Función de valoración de un individuo. - **Fv**
    - Función para decidir si el primer individuo es mejor que el segundo. - **Me** 
    
    También hemos creado unos nuevos tipos de datos para representar el problema y la solución obtenida. - **Solucion** y **Problema**

* **Tipos usados en Redes Neuronales** - *Línea 62* <br> Hemos creado dos nuevos tipos de datos para la creación de la red neuronal:
    - **Perceptrón** - Contendrá la información acerca del valor de su entrada ficticia, sus pesos actuales, su función de activación, y su función de activación derivada.
    - **Red** - Estará dividida en distintas capas, cada una asociada a un vector de perceptrones.

* **Funciones auxiliares** - *Línea 85* <br> Contiene una serie de funciones que serán utilizadas en los algoritmos principales.
    - **prob** - Genera una probabilidad aleatoria.
    - **sorteo** - Utilizado en el algoritmo de enfriamiento simulado.
    - **umbral** - Función umbral usada en la derivada de la función relu.
    - **mostrarCapa** - Muestra una capa de una red neuronal.
    - **mostrarCapaAux** - Función auxiliar para la función mostrarCapa.

* **Constructores** - *Línea 114*
    - **problema** - Crea datos de tipo *Problema*.
    - **red** - Devuelve una red neuronal a partir de unos parámetros de entrada.
    - **redAux** - Función auxiliar de *red*.
    - **redToArray** - Convierte una Red a un Array.
    - **arrayToRed** - Convierte un Array a una Red.
    - **arrayToRedAux** - Función auxiliar de *arrayToRed*.

* **Funciones de activación** - *Línea 151* <br> Funciones usadas en el algoritmo de Retropropagación
    - **sigmoide** - Implementación de la función Sigmoide.
    - **relu** - Implementación de la función ReLU.

* **Funciones de activación derivadas** - *Línea 161*
    - **dSigmoide** - Derivada de la función Sigmoide.
    - **dRelu** - Derivada de la función ReLU.
    
* **Algoritmo Búsqueda en Escalada** - *Línea 174*
    - **bEscalada** - Algoritmo de Búsqueda en escalada Simple.
    - **bEscaladaAux** - Función auxiliar de *bEscalada*.

* **Algoritmo Búsqueda en Escalada con Reinicio** - *Línea 199*
    - **bEscaladaReinicio** - Algoritmo de Búsqueda en escalada con Reinicio.
    - **bEscaladaReinicioAux** - Función auxiliar de *bEscaladaReinicio*.

* **Algoritmo Enfriamiento Simulado** - *Línea 226*
    - **eSimulado** - Algoritmo de Enfriamiento Simulado.
    - **eSimuladoEnfr** - Función auxiliar de *eSimulado* para el número de enfriamientos.
    - **eSimuladoIter** - Función auxiliar de *eSimulado* para el número de iteraciones.

* **Redes Neuronales** - *Línea 275*
    - **predecir** - A partir de una red y una entrada genera una salida.
    - **propHaciaAdelante** - Genera una lista con la asociaciones de los perceptrones con sus inputs.
    - **propHaciaAtrasAux** - Función auxiliar de *propHaciaAtras*.
    - **propHaciaAtras** - Genera los errores de cada capa respecto de su salida.
    - **retropropagacion** - Algoritmo de retropropagación.
    - **retropropagacionAux** - Función auxiliar de *retropropagacion*.

## **Elementos mínimos exigidos**
**2 funciones básicas de prelude y Data.List** <br>
*IA.hs - Línea 328*
```
predecir (O v r) ls = ...
    where
        ...
        nPesos = (P.length (pesos (v V.! 0))) - 1  
        calculaSalida i = P.sum $ ...
        ...
```
**2 funciones recursivas** <br>
*IA.hs - Línea 328*
```
propHaciaAtrasAux :: [[(Perceptron, Double)]] -> [Double] -> Double -> ([Double], Red)
propHaciaAtrasAux [ls] ys lr = ...

propHaciaAtrasAux (l:ls) ys lr = (variaciones, capa)                              
    where
        ...
        resultado = propHaciaAtrasAux ls ys lr                              
        ...        
```
*IA.hs - Línea 215*
```
bEscaladaReinicioAux :: Problema a -> Int -> Solucion a -> IO (Solucion a)
bEscaladaReinicioAux _ 0 sol = ...
bEscaladaReinicioAux p@(Problema _ _ _ me) nReinicios actual@(Solucion _ vAct) = do
    ...     
    if me (vSuc) (vAct)                                  
        then 
            bEscaladaReinicioAux p (nReinicios-1) sucesor   
        else
            bEscaladaReinicioAux p (nReinicios-1) actual 
```
**2 funciones por patrones** <br>
*IA.hs - Línea 247*
```
eSimuladoEnfr :: Problema a -> Double -> Double -> Int -> Int -> Solucion a -> Solucion a -> IO (Solucion a)
eSimuladoEnfr _ _ _ 0 _ _ mejor = ...
```
*IA.hs - Línea 215*
```
bEscaladaReinicioAux :: Problema a -> Int -> Solucion a -> IO (Solucion a)
bEscaladaReinicioAux _ 0 sol = ...
```
**2 usos de guardas** <br>
*IA.hs - Línea 392*
```
retropropagacion :: Red -> [([Double], [Double])] -> Int -> Double -> Red
retropropagacion red entrenamiento 0 lr = ...
retropropagacion red entrenamiento epochs lr
    | epochs <= 0   = ...
    | otherwise     = ...
```
*IA.hs - Línea 99*
```
umbral :: Double -> Double
umbral x
    |  x > 0 = ...
    |  x <= 0 = ...
```
**2 usos de case of** <br>
*IA.hs - Línea 125*
```
redAux :: Double -> Double -> [Int] -> [Double -> Double] -> [Double -> Double] -> Red
redAux w a cs (f:fs) (d:ds) = 
    case cs of
        [c] ->  ...
        (c:cs) -> ...
```
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

*IA.hs - Línea 145*
```
arrayToRedAux :: [Vector Perceptron] -> Red
arrayToRedAux ls = 
    case ls of
        [l]    -> ...
        (l:ls) -> ...
```
**2 usos de listas por compresión** <br>
*IA.hs - Línea 120*
```
red :: Double -> Double -> [Int] -> [Double -> Double] -> [Double -> Double] -> Red
red w a (c:cs) fs ds = E (V.fromList [ P 0 [ w | _ <- [1..p_sup]] id id | _ <- [1..c]]) (redAux w a cs fs ds)
    ...
```
*IA.hs - Línea 279*
```
predecir :: Red -> [Double] -> [Double]
predecir (S v) ls = salida  
    where
        salida = [ fActivacion (v V.! x) $ ls!!x + (eFicticia (v V.!x)) * P.head (pesos (v V.!x))  | x <- [0..(V.length v)-1  ]]
```
**2 usos de orden superior** <br>
*IA.hs - Línea 285*
```
predecir (O v r) ls = predecir r salida                  
    where
        ...
        calculaSalida i = P.sum $ P.map (\(a,w) -> a*w) $ ...
```    
*IA.hs - Línea 328*
```  
propHaciaAtrasAux :: [[(Perceptron, Double)]] -> [Double] -> Double -> ([Double], Red)
propHaciaAtrasAux [ls] ys lr = (variaciones, capa)                               
    where
        variaciones = P.map (\i -> derivada i * diferencia i) [0..(P.length ls)-1] 
        ...
```  
**2 usos de evaluación perezosa** <br>
*TSPAndalucia.hs - Línea 40*
```
intercambiaPosiciones :: (Int, Int) -> Mapa -> Mapa
intercambiaPosiciones (a, b) xs = take (length xs) [ selecciona i | i <- [0..] ]
    ...
```
*TSPAndalucia.hs - Línea 71*
```
fValoracion :: Mapa -> Double
fValoracion ls = ...
    where
        ls' = take (length ls-1) [((ls!!i), (ls!!(i+1))) | i <- [0..]]
```
**Creación de un módulo** <br>
*IA.hs - Línea 8*
```
module IA 
(
    Ind,
    Val,
    ...
```
<br>
<br>
<br>

**2 tipos de datos nuevos y usos de éstos.** <br>
*IA.hs - Línea 40 y 371*
```
data Perceptron = P {   
                        eFicticia :: Double,        
                        ...
                    }
...
propHaciaAtras :: [[(Perceptron, Double)]] -> [Double] -> Double -> Red
```
*IA.hs - Línea 56 y 179*
```
data Problema a = Problema (Gi a) (Gs a) (Fv a) Me
...
bEscalada :: Problema a -> IO (Solucion a)
bEscalada p@(Problema gi gs fv me) = do
    ...
```
**2 tipos de datos abstractos o librerías vistos en la asignatura** <br>
*IA.hs - Línea 75* - Vector
```
data Red    = E (Vector Perceptron) Red  
            | ...
```
*IA.hs - Línea 141* - Array
```
arrayToRed :: Array Int (Vector Perceptron) -> Red
arrayToRed ls = E vs (arrayToRedAux vss)
    where
        (vs:vss) = A.elems ls
```

## **Ejemplos propuestos**

**Búsqueda en escalada**

Para resolver un problema mediante búsqueda en escalada, es necesario modelar antes el problema mediante el tipo de datos *Problema*.

Cargamos el archivo "TSP.hs" de la siguiente manera:
```
ghci ./ejemplos/TSP.hs
```

Disponemos del problema del viajante en un cuadrado de N puntos ya modelado.

El problema modelado dispone de las siguientes funciones:

- La función "generaInicial", que genera una lista de 4*N puntos en un orden aleatorio. Por ejemplo:
```
inicial <- generaInicial
inicial

```

- La función "generaSucesor", que genera un estado sucesor a partir de otro. En este caso, el sucesor se calcula haciendo un intercambio de posiciones. Por ejemplo:
```
inicial <- generaInicial
inicial
sucesor <- generaSucesor inicial
sucesor

```

- La función "fValoracion", que da la valoración de un estado. En este caso, la valoración es la suma de la distancia euclidea de los puntos. Por ejemplo:
```
inicial <- generaInicial
valoracion = fValoracion inicial
valoracion

```

- La función "Mejor", que decide si una valoración es mejor que otra. En este caso, la primera valoración es mejor que la segunda si la primera valoración es menor. Por ejemplo:
```
inicial <- generaInicial
vInicial = fValoracion inicial
sucesor <- generaSucesor inicial
vSucesor = fValoracion sucesor
sucesorMejorQueInicial = mejor vSucesor vInicial
sucesorMejorQueInicial

```

Para ver la solución final tras ejecutar el algoritmo de búsqueda en escalada, basta con llamar a la siguiente función:
```
solucionBusquedaEscalada
```
Que, internamente, realiza la siguiente ejecución:
```
bEscalada problemaTsp
```
Donde el primer parámetro es el problema modelado.

Una posible salida es:
```
Solución: [(2,5),(5,1),(3,0),(5,0),(0,4),(5,3),(2,0),(0,3),(5,5),(0,1),
            (0,2),(1,0),(0,0),(4,0),(5,2),(1,5),(0,5),(5,4),(3,5),(4,5)]
Valoración: 67.1819161817682
```

### **Búsqueda en escalada con reinicio**
Para resolver un problema mediante búsqueda en escalada con reinicio, es necesario modelar antes el problema mediante el tipo de datos *Problema*.

Cargamos el archivo "TSPAndalucia.hs" de la siguiente manera:
```
ghci ./ejemplos/TSPAndalucia.hs
```

Disponemos del problema del viajante con las provincias de Andalucía ya modelado.

El problema modelado dispone de las mismas funciones que el poblema del TSP, por lo que se pueden ejecutar los mismos comandos.

Para ver la solución final tras ejecutar el algoritmo de búsqueda en escalada con reinicio, basta con llamar a la siguiente función:
```
solucionBusquedaEscaladaReinicio
```
Que, internamente, realiza la siguiente ejecución:
```
bEscaladaReinicio problemaTsp 100
```
Donde el primer parámetro es el problema modelado, y el segundo es el número de reinicios.

Una posible salida es:
```
Solución: [("sevilla",(90.0,153.0)),("cordoba",(198.0,207.0)),("jaen",(295.5,192.0)),("almeria",(409.5,93.0)),
            ("granada",(309.0,127.5)),("malaga",(232.5,75.0)),("cadiz",(63.0,57.0)),("huelva",(3.0,139.5))]
Valoración: 929.9255755927757
```

### **Enfriamiento simulado**
Para resolver un problema mediante enfriamiento simulado, es necesario modelar antes el problema mediante el tipo de datos *Problema*.

Cargamos el archivo "TSP.hs" de la siguiente manera:
```
ghci ./ejemplos/TSP.hs
```

Disponemos del problema del viajante en un cuadrado de N puntos ya modelado, .


Para ver la solución final tras ejecutar el algoritmo de enfriamiento simulado, basta con llamar a la siguiente función:
```
solucion
```
Que, internamente, realiza la siguiente ejecución:
```
eSimulado problemaTsp 100 0.1 100 100
```
Donde el primer parámetro es el problema modelado, el segundo es la temperatura inicial, el tercero es el factor descenso, el cuarto es el número de enfriamientos, y el quinto es el número de iteraciones.

Una posible salida es (solución óptima):
```
Solución: [(0,3),(0,2),(0,1),(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(5,1),
            (5,2),(5,3),(5,4),(5,5),(4,5),(3,5),(2,5),(1,5),(0,5),(0,4)]
Valoración: 20.0
```

### **Redes neuronales**
Cargando el archivo "votos.hs" de la siguiente manera:
```
ghci ./ejemplos/votos.hs
```

Disponemos de un ejemplo de creación de redes neuronales, predicción de valores, y entrenamiento de redes mediante el algoritmo de retropropagación.

Para crear una red idéntica a la del ejemplo, bastaría con ejecutar lo siguiente:
```
pesos = 1
eFicticias = 1
neuronas = [16,4,2,1]
activacion = [sigmoide, sigmoide,sigmoide]
dActivacion = [dSigmoide, dSigmoide, dSigmoide]
redEjemplo = red pesos eFicticias neuronas activacion dActivacion
redEjemplo

```
Que crea una red neuronal con 16 neuronas en la capa de entrada, 4 en la primera capa oculta, 2 en la segunda, y 1 en la capa de salida. Los pesos iniciales de todas las neuronas son 1, las entradas fictias son 1, y las funciones de activación de la primera capa oculta, segunda, y capa de salida son: sigmoide, sigmoide, sigmoide. Las derivadas de las funciones de activación se situan de la misma manera.

Para predecir valores con la red del ejemplo, bastaría con ejecutar lo siguiente:
```
predecir redVotos [-1, -1, 1, -1, -1, 1, 1, 1, 1, 1, 1, -1, 1, 1, -1, 1]

```
Que genera la probabilidad de que un voto sea republicano (1) o demócrata (0).

Para entrenar la red, hay que ejecutar lo siguiente:
```
redEntrenada = retropropagacion redVotos conjEntrenamiento 100 0.1
redEntrenada

```

<br>
<br>
<br>
<br>

Donde el primer parámetro es una red, el segundo un conjunto de entrenamiento (de la forma [([entrada],[salida esperada])]), el tercero es el número de épocas (epochs), y el cuarto es la tasa de aprendizaje.

Para ver el problema resuelto de manera más bonita, ejecute lo siguiente:
```
estadisticas 100 0.1
```
Donde el primer parámetro es el número de épocas, y el segundo la tasa de aprendizaje