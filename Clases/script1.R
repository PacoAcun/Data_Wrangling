install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("stringr")
install.packages("readr")

require()
library(dplyr)

## Funciones importantes
?filter #Ver documentacion
getwd() #location
setwd() #location de donde quiero utilizar mis archivos
head()  #primeros x records
str() #lista la estructura de la data
length() #tamaño del objeto

## Tippos de datos
string <- "this is a string"
string
class(string)
length(string)
nchar(string)

double <- 234
class(double)
typeof(double)

f1 <- 1/8+54
f1
typeof(f1)

int <- 3L
int
typeof(int)

bool <- FALSE
bool
bool*1
as.logical(1)

## Vector: Coleccion de elementod de un mismo tipo de data.
c(1,2,3)
length(c(1,2,3))
vec1 <- 1:7
vec1
vec1*2
vec2 <- sample(letters, 7, replace = FALSE)

sample(1:5, 7, replace = TRUE)
