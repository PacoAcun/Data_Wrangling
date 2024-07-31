#### cargar librerias

library(readr)
library(readxl)
library(openxlsx)

install.packages("openxlsx")

#### Ejemplo 1: Cargar Archivos de Excel
leer_excel <- read_excel('ejemplo_xlsx/example_1.xlsx', sheet = 2)
head(leer_excel)

#### Ejemplo 2: Cargar Archivos de csv
leer_csv <- read_csv("ejemplo_xlsx/example_2.csv")
head(leer_csv)

#### Ejemplo 3: Cargar Archivos de txt
leer_txt <- read_delim("ejemplo_xlsx/example_3.txt", delim = ';')
head(leer_txt)

#### Ejemplo 4: Cargar Archivos de txt
leer_txt <- read_delim("ejemplo_xlsx/example_4.txt", delim = '|')
head(leer_txt)

#### Ejemplo 5: Exportar un data set a excel
write.xlsx(leer_txt, 'ejemplo_xlsx/excel_exportado.xlsx')