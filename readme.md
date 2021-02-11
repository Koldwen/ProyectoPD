# **Cómo usar la librería IA (Inteligencia Artificial)**

*Si utilizas Visual Code y quieres verlo en un formato correcto puedes hacer clic derecho sobre Entrega.md y pulsar en **Open Preview*** 

*También puedes pulsar **Ctrl+Mayús+V***

## **Importación de la librería.**

```
import IA
```
 
## **Ejecución de ejemplos.**

### Ejemplos
* [TSP](#tsp) - TSP (Travelling Salesman Problem) con un cuadrado de N puntos.
* [TSPAndalucia](#tsp-andalucia) - TSP (Travelling Salesman Problem) con las provincias de Andalucía.
* [Votos](#votos) - Problema de clasificación de votos con redes neuronales.

### **TSP**
**Cargar el documento**
```
ghci ./ejemplos/TSP.hs
```
**Solución mediante Búsqueda en Escalada**
```
solucionBusquedaEscalada
```
**Solución mediante Búsqueda en Escalada con reinicio**
```
solucionBusquedaEscaladaReinicio
```
**Solución mediante Enfiramiento Simulado**
```
solucionEnfriamientoSimulado
```

### **TSP Andalucia**
**Cargar el documento**
```
ghci ./ejemplos/TSPAndalucia.hs
```
**Solución mediante Búsqueda en Escalada**
```
solucionBusquedaEscalada
```
**Solución mediante Búsqueda en Escalada con reinicio**
```
solucionBusquedaEscaladaReinicio
```
**Solución mediante Enfiramiento Simulado**
```
solucionEnfriamientoSimulado
```

### **Votos**
**Cargar el documento**
```
ghci ./ejemplos/votos.hs
```

**Estadísticas del entrenamiento de la red**\
Por ejemplo, si queremos ejecutar el entrenamiento con epochs igual a 100, y una tasa de aprendizaje de 0.1:

```
estadisticas 100 0.1
```

**Predicción de valores**\
Por ejemplo, si queremos clasificar el voto [1,-1,1,-1,-1,0,1,1,1,-1,0,0,-1,0,0,0]:
```
predecir redVotos [1,-1,1,-1,-1,0,1,1,1,-1,0,0,-1,0,0,0]
```