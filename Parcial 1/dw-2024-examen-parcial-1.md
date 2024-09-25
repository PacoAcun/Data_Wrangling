dw-2024-parcial-1
================
Tepi
25/09/2024

# Examen parcial

Indicaciones generales:

- Usted tiene el período de la clase para resolver el examen parcial.

- La entrega del parcial, al igual que las tareas, es por medio de su
  cuenta de github, pegando el link en el portal de MiU.

- Pueden hacer uso del material del curso e internet (stackoverflow,
  etc.). Sin embargo, si encontramos algún indicio de copia, se anulará
  el exámen para los estudiantes involucrados. Por lo tanto, aconsejamos
  no compartir las agregaciones que generen.

## Sección 0: Preguntas de temas vistos en clase (20pts)

- Responda las siguientes preguntas de temas que fueron tocados en
  clase.

1.  ¿Qué es una ufunc y por qué debemos de utilizarlas cuando
    programamos trabajando datos?

R/. Es una funcion de una bibloteca que permite hacer operaciones a todo
un vector o matriz, es mejor utilizarlo ya que hace que se puedan hacer
operaciones a toda la matriz sin necesidad de ir por cada uno de los
elementos.

2.  Es una técnica en programación numérica que amplía los objetos que
    son de menor dimensión para que sean compatibles con los de mayor
    dimensión. Describa cuál es la técnica y de un breve ejemplo en R.

R/. la tecnica se llama broadcasting y hace que el vector mas corto sea
de la misma longitud que el vector mas largo para que estos se puedan
operar.

``` r
vector_1 <- c(1, 2, 3)
vector_2 <- c(4, 5)

resultado <- vector_1 + vector_2
```

    ## Warning in vector_1 + vector_2: longer object length is not a multiple of
    ## shorter object length

``` r
print(resultado)
```

    ## [1] 5 7 7

3.  ¿Qué es el axioma de elegibilidad y por qué es útil al momento de
    hacer análisis de datos?

R/. Es cuando para hacer un analisis correcto se deben de usar los datos
que sean validos o legibles, de esta forma se asegura que el analisis
realizado sea importante para el objetivo por lo que los datos elegidos
deben cumplir ciertos requisitos.

4.  Cuál es la relación entre la granularidad y la agregación de datos?
    Mencione un breve ejemplo. Luego, exploque cuál es la granularidad o
    agregación requerida para poder generar un reporte como el
    siguiente:

| Pais | Usuarios |
|------|----------|
| US   | 1,934    |
| UK   | 2,133    |
| DE   | 1,234    |
| FR   | 4,332    |
| ROW  | 943      |

R/. La granularidad se refiere a lo detallado del reporte mientras que
la agregacion se refiere a cuando se combinan datos para mostrarlos de
manera mas resumida en un reporte, en este caso la columna pais es la
granularidad de este reporte ya que detalla que y cuantos paises hay,
mientras que la agregacion seria la columna de Usuarios ya que puede que
en la base de datos existieran mas columnas como genero, rango de
edades, etc, por lo que al combinar todas estas en una columna llamada
usuarios se resumen los datos para el reporte, sirve para darle
importancia a los detalles que se buscan sin agregar datos que no
importen para el objetivo requerido.

## Sección I: Preguntas teóricas. (50pts)

- Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
  deberá responder 5. Las 5 a responder estarán determinadas por un
  muestreo aleatorio basado en su número de carné.

- Ingrese su número de carné en `set.seed()` y corra el chunk de R para
  determinar cuáles preguntas debe responder.

``` r
set.seed(20220565) 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 4, 7, 9, 10"

``` r
# Preguntas: 1, 4, 7, 9, 10
```

### Listado de preguntas teóricas

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:
    - `str()`
    - `df[,c("a","b")]`
    - `names(df)[4] <- "new_name"` donde la posición 4 corresponde a la
      variable `old_name`
    - `df[df$variable == "valor",]`

R/.  
\* library(dplyr) \* df %\>% \* select(a, b)

4.  ¿Cuál es la diferencia entre utilizar `==` y `=` en R?

R/. Uno se usa para comparar valores (==), y el otro se usa para asignar
una variable a un valor (=)

7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes?

R/. Utilizando la funcion de levels() puedes crear otros niveles que no
se encuentran y se definen al principio.

9.  En SQL, ¿para qué utilizamos el keyword `HAVING`?

R/. Sirve para filtrar los datos despues de operarlos y solo se usan en
grupos.

10. Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?

    - SELECT \* FROM A LEFT JOIN B ON A.KEY = B.KEY WHERE B KEY = NULL

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

``` r
Preguntas <- 10
x <- 5   

num_examenes <- choose(preguntas, x)

print(num_examenes)
```

    ## [1]   0   0  21 126 252

## Sección II Preguntas prácticas. (30pts)

- Conteste las siguientes preguntas utilizando sus conocimientos de R.
  Adjunte el código que utilizó para llegar a sus conclusiones en un
  chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### II. Preguntas Practicas

## A

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- readRDS("parcial_anonimo.rds")

data <- data %>%
  rename(Unidades_plaza = `Unidades plaza`)

clientes_mult_pais <- data %>%
  group_by(Cliente) %>%
  summarize(paises_unicos = n_distinct(Pais)) %>%
  filter(paises_unicos > 1)

rentabilidad_clientes <- data %>%
  filter(Cliente %in% clientes_mult_pais$Cliente) %>%
  group_by(Cliente) %>%
  summarize(total_unidades = sum(Unidades_plaza, na.rm = TRUE),
            total_venta = sum(Venta, na.rm = TRUE))

cliente_mas_rentable <- rentabilidad_clientes %>%
  arrange(desc(total_venta)) %>%
  slice(1)

print(cliente_mas_rentable)
```

    ## # A tibble: 1 × 3
    ##   Cliente  total_unidades total_venta
    ##   <chr>             <dbl>       <dbl>
    ## 1 a17a7558           2274      19818.

Primero al identificar los clientes que pertenecen a mas de 1 pais y
luego se agrupan para saber cual de esos clientes tiene el total de
ventas mas alto por lo que el cliente a17a7558 es el mas rentable lo que
significa que los paises donde este cliente esta presente tiene buena
aceptacion del producto, por lo que se recomienda priorizar a este
cliente en este caso.

## B

``` r
library(dplyr)

data <- readRDS("parcial_anonimo.rds")

data <- data %>%
  rename(Unidades_plaza = `Unidades plaza`)

territorio_rentabilidad <- data %>%
  group_by(Territorio, Pais) %>%
  summarize(total_unidades = sum(Unidades_plaza, na.rm = TRUE),
            total_venta = sum(Venta, na.rm = TRUE),
            perdida = sum(Venta[Venta < 0], na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(precio_unitario = ifelse(total_unidades > 0, total_venta / total_unidades, NA))

territorios_con_perdidas <- territorio_rentabilidad %>%
  filter(perdida < 0) %>%
  arrange(perdida)

print("Territorios con pérdidas:")
```

    ## [1] "Territorios con pérdidas:"

``` r
print(territorios_con_perdidas)
```

    ## # A tibble: 79 × 6
    ##    Territorio Pais     total_unidades total_venta perdida precio_unitario
    ##    <chr>      <chr>             <dbl>       <dbl>   <dbl>           <dbl>
    ##  1 f7dfc635   4046ee34          73483     916786. -14985.           12.5 
    ##  2 77192d63   4046ee34          20702     247252   -5641.           11.9 
    ##  3 72520ba2   4f03bd9b          42957     356377.  -3761.            8.30
    ##  4 69c1b705   4046ee34          11988     152545.  -3370.           12.7 
    ##  5 1d407777   4046ee34          16005     204601.  -3300.           12.8 
    ##  6 bc8e06ed   4046ee34          21130     329853.  -3269.           15.6 
    ##  7 2e812869   4046ee34          10801     138780.  -3056.           12.8 
    ##  8 67e9cc18   4046ee34           9710     126771.  -2721.           13.1 
    ##  9 8f79b7f8   4f03bd9b          10822     114890.  -1858.           10.6 
    ## 10 a0d39798   4f03bd9b          50681     441722.  -1779.            8.72
    ## # ℹ 69 more rows

Los datos proporcionan una visión clara sobre el rendimiento financiero
de tres territorios, destacando tanto las ventas como las pérdidas
asociadas. En el primer territorio 72520ba2, a pesar de generar un total
de ventas de 356,377.19, se observa una pérdida de -3,760.95, lo que
sugiere que, aunque las ventas son significativas, los costos o
devoluciones han impactado negativamente su rentabilidad, resultando en
un precio unitario de 8.30
