library(nycflights13)
library(dplyr)
library(lubridate)

##### Funciones Basicas

#### Fecha de hoy o justo en este momento
today()
now()

#### Generar una variable de fecha
make_date(year = 2024, month = 10, day = 07)

#### Prsear fechas
x <- "2004 February 11th"
ymd(x)

y <- "11.02.2004"
dmy(y)

##### Diferencia de tiempo
# La fecha de aterrizaje a la luna
date_landing <- mdy("July 20, 1969")
moment_step <- mdy_hms("July 20, 1969, 02:56:15", tz = "UTC")
##### Cuántos dias desde el primer aterrizaje en la luna
difftime(today(), date_landing, units = "days")

difftime(now(), moment_step, units="secs")


##### nycflights13
View(flights)
