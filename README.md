# TP ALP - Matías Nores

## **Requisitos previos**

Antes de utilizar el intérprete de Álgebra Relacional, asegúrese de tener instalado el compilador de Haskell **GHC** y la herramienta **cabal** para gestión de paquetes.

* [GHCup](https://www.haskell.org/ghcup/)

## Dependencias 
Este proyecto utiliza librerías estándar y algunas externas para el parsing y la interfaz de línea de comandos. Ejecuta el siguiente comando para instalar las dependencias necesarias:

```
cabal update
cabal install --lib parsec containers haskeline
```

## **Usar con GHC y GHCI**
Se usó Stack para las dependencias y facilidad de compilación y ejecución. Para instalarlo fíjese en (https://docs.haskellstack.org/en/stable/README/#how-to-install) para encontrar guías de instalación en distintas plataformas. Una vez instalado abra una terminal en el directorio del programa y ejecute:
```
stack setup
```

Para correr en GHCI escribir en el directorio correspondiente:
```
stack ghci
```
Una vez compilado y sin errores (por ahí hayan algunos warnings de funciones parciales y de instanciación),
correr en GHCI la funcion **main** para entrar en el loop de la CLI.

Para usar el intérprete con GHC correr:
```
stack build
```
Esto genera el ejecutable, correr en la terminal como:
```
stack run
```
Ahora **stack run** corre el servidor de la web en local (puerto 3000), abrir en

* [URL]:(http://localhost:3000/studio)

## **Dentro de la CLI**
Ya dentro de la CLI del intérprete, se podrá empezara usar el mismo junto a una base de datos que fue
hardcodeada (usada en TBD). Las funcionalidades que provee el intérprete son:
```
reset                 elimina asignaciones del catálogo  
getcat                muestra en pantalla el catálogo

SELECT (P) T          selecciona las filas de la tabla T que cumplent el predicado P
PROJECT [ATR] T       proyecta las columnas ATR de la tabla T
RENAME X T            devuelve una tabla igual a T con sus atributos renombrados con X
GROUP [G] (A) T       agrupa los atributos en G de T con las funciones de agregado en A
X <- EXP              crea una nueva tabla X con el resultado de evaluar EXP
R U S                 une las filas de ambas tablas, sin duplicados
R $ S                 devuelve las filas que están en ambas tablas
R - S                 devuelve las filas de R que no están en S
R X S                 producto cartesiano entre tablas
R |X| S               producto natural entre tablas
R / S                 division entre tablas
```
Algunas cosas a tener en cuenta para escribir consultas:

* Las referencias a tablas se hacen sin comillas (tabla T, no "T")
* Lo único que se pone entre comillas (dobles) son valores de tipo string en predicados
* Los predicados se escriben entre paréntesis, usando operadores lógicos AND, NOT y OR, y de comparación >, <, >=, <=, = y <>. Ejemplo:
```
SELECT (EDAD > 7 AND CURSO = "Tercero") ESTUDIANTES
```
* Los atributos proyectados se escriben entre corchetes y sin comillas. Ejemplo:
```
PROJECT [NOMBRE, EDAD] ESTUDIANTES
```
* Los grupos para agrupamiento van entre corchetes, las funciones de agrupamiento entre paréntesis. Ej:
```
GROUP [SID] (SUM CANT AS Total_Piezas, COUNT AS Refs) SPJ
```
* Tener en cuenta que las consultas de asignación (**<-**) guardan el resultado en el catálogo, incluso si se trata de una tabla vacía
* Se usa el símbolo **$** para intersección por comodidad (no tener que estar haciendo copy-paste todo el tiempo) 
* Cabe aclarar que se puede cambiar la precedencia usando paréntesis para priorizar operaciones
* En la carpeta **ejemplos** encontrará archivos con consultas que pueden probar o usar de modelo para hacer sus propias consultas, así como también otros comandos de CLI que tiran errores de tipo o referencia a modo de prueba
