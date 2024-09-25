# Instalar paquetes necesarios (si no están instalados)
# install.packages(c("dplyr", "ggplot2", "readr", "stringr"))

# Cargar librerías
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Leer los datos desde el archivo CSV con codificación UTF-8
data <- read_csv("Laboratorio 4/tabla_completa.csv", locale = locale(encoding = "UTF-8"))

# Reemplazar caracteres especiales en datos
data$UNIDAD <- str_replace_all(data$UNIDAD, "Ã³", "ó")
data$UNIDAD <- str_replace_all(data$UNIDAD, "Ã­n", "ín")

# Mostrar las primeras filas para verificar la carga
head(data)

# Análisis de la carga por piloto

# Agrupar por piloto y calcular la cantidad total de carga manejada
piloto_summary <- data %>%
  group_by(PILOTO) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD),
    Total_Credito_Dias = sum(CREDITO), # Total de días de crédito
    Num_Viajes = n()
  ) %>%
  arrange(desc(Total_Cantidad))

# Mostrar resumen como tabla
print("Resumen de Carga por Piloto:")
print(piloto_summary)

# Gráfico de carga total por piloto
ggplot(piloto_summary, aes(x = reorder(PILOTO, Total_Cantidad), y = Total_Cantidad)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Carga Total por Piloto", x = "Piloto", y = "Carga Total")

# Análisis de la necesidad de más vehículos

# Agrupar por tipo de unidad y calcular la cantidad total de carga
unidad_summary <- data %>%
  group_by(UNIDAD) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD),
    Total_Credito_Dias = sum(CREDITO), # Total de días de crédito por tipo de unidad
    Num_Viajes = n()
  ) %>%
  arrange(desc(Total_Cantidad))

# Mostrar resumen como tabla
print("Resumen de Carga por Tipo de Unidad:")
print(unidad_summary)

# Gráfico de carga total por tipo de unidad
ggplot(unidad_summary, aes(x = reorder(UNIDAD, Total_Cantidad), y = Total_Cantidad)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Carga Total por Tipo de Unidad", x = "Tipo de Unidad", y = "Carga Total")

# Revisión de tarifas actuales  

# Agrupar por cliente y calcular el monto total facturado (Q)
cliente_summary <- data %>%
  group_by(CLIENTE) %>%
  summarise(
    Total_Monto = sum(Q),  # Sumar el total facturado (columna Q)
    Num_Viajes = n()
  ) %>%
  arrange(desc(Total_Monto))

# Mostrar resumen como tabla
print("Resumen de Monto por Cliente:")
print(cliente_summary)

# Gráfico de monto total por cliente
ggplot(cliente_summary, aes(x = reorder(CLIENTE, Total_Monto), y = Total_Monto)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Monto Total por Cliente", x = "Cliente", y = "Monto Total")

# Revisión de posibles registros sospechosos por pilotos

# Filtrar registros con "Faltante" o "Devolución" en la columna CLIENTE
suspicious_activity <- data %>%
  mutate(CLIENTE = str_to_lower(CLIENTE)) %>%  # Convertir a minúsculas para evitar problemas con mayúsculas/minúsculas
  filter(str_detect(CLIENTE, "faltante") | str_detect(CLIENTE, "devolucion")) %>%
  group_by(PILOTO, CLIENTE) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD),
    Total_Credito_Dias = sum(CREDITO), # Total de días de crédito
    Num_Viajes = n()
  ) %>%
  arrange(desc(Total_Cantidad))

# Mostrar los datos filtrados para verificar si hay registros
print("Datos de Actividad Sospechosa por Piloto:")
print(suspicious_activity)

# Si hay registros, mostrar el gráfico
if (nrow(suspicious_activity) > 0) {
  # Gráfico de actividad sospechosa por piloto
  ggplot(suspicious_activity, aes(x = reorder(PILOTO, Total_Cantidad), y = Total_Cantidad, fill = CLIENTE)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Actividad Sospechosa por Piloto", x = "Piloto", y = "Cantidad Total")
} else {
  print("No se encontraron actividades sospechosas para los criterios definidos.")
}

# Mostrar resumen como tabla
print("Resumen de Actividad Sospechosa por Piloto:")
print(suspicious_activity)

# Gráfico de actividad sospechosa por piloto
ggplot(suspicious_activity, aes(x = reorder(PILOTO, Total_Cantidad), y = Total_Cantidad, fill = CLIENTE)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Actividad Sospechosa por Piloto", x = "Piloto", y = "Cantidad Total")

# Análisis 80-20 de Clientes

# Calcular el porcentaje acumulado
cliente_summary <- cliente_summary %>%
  mutate(
    Cumulative_Monto = cumsum(Total_Monto),
    Total_Monto_Sum = sum(Total_Monto),
    Percentage = Cumulative_Monto / Total_Monto_Sum * 100
  )

# Determinar el punto de corte para el 80-20
threshold <- cliente_summary %>%
  filter(Percentage <= 85) %>%
  tail(1)

# Filtrar los clientes que contribuyen al 80%
top_customers <- cliente_summary %>%
  filter(Cumulative_Monto <= threshold$Cumulative_Monto)

# Mostrar resumen como tabla
print("Clientes que Contribuyen al 80% del Monto Total:")
print(top_customers)

# Gráfico del análisis 80-20 (eje x e y intercambiados)
ggplot(cliente_summary, aes(x = Total_Monto, y = reorder(CLIENTE, Total_Monto))) +
  geom_bar(stat = "identity") +
  geom_line(aes(x = Cumulative_Monto, y = reorder(CLIENTE, Cumulative_Monto)), color = "blue", size = 1) +
  labs(title = "Análisis 80-20 de Clientes", x = "Total Monto", y = "Cliente") +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5))

# Agrupar por piloto y calcular la cantidad total de carga y crédito
piloto_summary <- data %>%
  group_by(PILOTO) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD),
    Total_Credito_Dias = sum(CREDITO), # Total de días de crédito
    Num_Viajes = n()
  ) %>%
  arrange(desc(Total_Cantidad))

# Mostrar resumen como tabla
print("Resumen de Mejores Pilotos:")
print(piloto_summary)

# Visualización de los mejores pilotos
ggplot(piloto_summary, aes(x = reorder(PILOTO, Total_Cantidad), y = Total_Cantidad)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Mejores Pilotos por Carga Total", x = "Piloto", y = "Carga Total")

# Agrupar por unidad y calcular la cantidad total de carga y crédito
unidad_summary <- data %>%
  group_by(UNIDAD) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD),
    Total_Credito_Dias = sum(CREDITO), # Total de días de crédito por tipo de unidad
    Num_Viajes = n()
  ) %>%
  arrange(desc(Total_Cantidad))

# Mostrar resumen como tabla
print("Resumen de Transportes Más Efectivos:")
print(unidad_summary)

# Visualización de los transportes más efectivos
ggplot(unidad_summary, aes(x = reorder(UNIDAD, Total_Cantidad), y = Total_Cantidad)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Transportes Más Efectivos por Carga Total", x = "Tipo de Unidad", y = "Carga Total")
# Agrupar por piloto y calcular la cantidad total de carga manejada
piloto_summary <- data %>%
  group_by(PILOTO) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD, na.rm = TRUE),   # Sumar la cantidad de carga
    Total_Credito_Dias = sum(CREDITO, na.rm = TRUE), # Sumar los días de crédito
    Num_Viajes = n()                               # Contar el número de viajes
  ) %>%
  arrange(desc(Total_Cantidad))

# Mostrar el resumen de carga por piloto
print("Resumen de Carga por Piloto:")
print(piloto_summary)

# Calcular el promedio de la carga y el número de viajes actual
promedio_cantidad <- mean(piloto_summary$Total_Cantidad)
promedio_viajes <- mean(piloto_summary$Num_Viajes)

# Proyectar un crecimiento del 10% en la demanda de carga y viajes
crecimiento <- 1.10
piloto_summary <- piloto_summary %>%
  mutate(
    Proyeccion_Cantidad = Total_Cantidad * crecimiento,
    Proyeccion_Viajes = Num_Viajes * crecimiento
  )

# Resumen de la capacidad proyectada
resumen_pilotos <- piloto_summary %>%
  summarise(
    Total_Cantidad_Actual = sum(Total_Cantidad),
    Total_Viajes_Actual = sum(Num_Viajes),
    Total_Cantidad_Proyectada = sum(Proyeccion_Cantidad),
    Total_Viajes_Proyectados = sum(Proyeccion_Viajes)
  )

# Mostrar el resumen de la capacidad operativa y proyecciones
print("Resumen de la Capacidad Operativa y Proyecciones:")
print(resumen_pilotos)

# Imprimir tabla para gráfico de carga proyectada por piloto
print("Tabla de Datos para Gráfico de Carga Proyectada por Piloto:")
print(piloto_summary %>% select(PILOTO, Total_Cantidad, Proyeccion_Cantidad))

# Gráfico de carga total proyectada por piloto
ggplot(piloto_summary, aes(x = reorder(PILOTO, Proyeccion_Cantidad), y = Proyeccion_Cantidad)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Carga Total Proyectada por Piloto", x = "Piloto", y = "Carga Total Proyectada")

# Imprimir tabla para gráfico de viajes proyectados por piloto
print("Tabla de Datos para Gráfico de Viajes Proyectados por Piloto:")
print(piloto_summary %>% select(PILOTO, Num_Viajes, Proyeccion_Viajes))

# Gráfico de viajes proyectados por piloto
ggplot(piloto_summary, aes(x = reorder(PILOTO, Proyeccion_Viajes), y = Proyeccion_Viajes)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Viajes Proyectados por Piloto", x = "Piloto", y = "Número de Viajes Proyectados")

# Agrupar por mes y calcular la cantidad total de carga y el número de viajes
monthly_summary <- data %>%
  group_by(MES) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD, na.rm = TRUE),
    Total_Viajes = n(),
    .groups = 'drop'
  ) %>%
  arrange(MES)

# Agrupar por mes y calcular la cantidad total de carga y el número de viajes
monthly_summary <- data %>%
  group_by(MES) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD, na.rm = TRUE),
    Total_Viajes = n(),
    .groups = 'drop'
  ) %>%
  arrange(MES)

# Calcular el crecimiento mensual en carga y viajes en términos absolutos
monthly_summary <- monthly_summary %>%
  mutate(
    Growth_Cantidad = Total_Cantidad - lag(Total_Cantidad, default = first(Total_Cantidad)),
    Growth_Viajes = Total_Viajes - lag(Total_Viajes, default = first(Total_Viajes))
  )

# Calcular el porcentaje de crecimiento mensual
monthly_summary <- monthly_summary %>%
  mutate(
    Percent_Growth_Viajes = (Growth_Viajes / lag(Total_Viajes, default = first(Total_Viajes))) * 100
  )

# Mostrar la tabla de crecimiento mensual con porcentajes
print("Tabla de Crecimiento Mensual con Porcentajes:")
print(monthly_summary)

# Mostrar el resumen de crecimiento anual (opcional)
annual_summary <- monthly_summary %>%
  summarise(
    Total_Crecimiento_Cantidad = sum(Growth_Cantidad, na.rm = TRUE),
    Total_Crecimiento_Viajes = sum(Growth_Viajes, na.rm = TRUE),
    Percent_Growth_Viajes = (Total_Crecimiento_Viajes / first(Total_Viajes)) * 100
  )

print("Resumen de Crecimiento Anual:")
print(annual_summary)


# Agrupar por mes y calcular la cantidad total de carga y el número de viajes
monthly_summary <- data %>%
  group_by(MES) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD, na.rm = TRUE),
    Total_Viajes = n(),
    .groups = 'drop'
  ) %>%
  arrange(MES)

# Calcular el crecimiento mensual en carga y viajes en términos absolutos
monthly_summary <- monthly_summary %>%
  mutate(
    Growth_Cantidad = Total_Cantidad - lag(Total_Cantidad, default = first(Total_Cantidad)),
    Growth_Viajes = Total_Viajes - lag(Total_Viajes, default = first(Total_Viajes))
  )

# Calcular el promedio de crecimiento mensual en el número de viajes
average_growth_viajes <- monthly_summary %>%
  summarise(
    Avg_Growth_Viajes = mean(Growth_Viajes, na.rm = TRUE)
  )

# Inicializar valores para proyección
last_month <- max(monthly_summary$MES)
last_total_viajes <- tail(monthly_summary$Total_Viajes, 1)
last_total_cantidad <- tail(monthly_summary$Total_Cantidad, 1)

# Crear un dataframe para las proyecciones
projection <- data.frame(
  MES = sprintf("%02d", seq(as.numeric(last_month) + 1, as.numeric(last_month) + 24)),
  Proj_Total_Viajes = NA,
  Proj_Total_Cantidad = NA
)

# Realizar la proyección para los próximos 24 meses
for (i in 1:nrow(projection)) {
  projection$Proj_Total_Viajes[i] <- last_total_viajes + i * average_growth_viajes$Avg_Growth_Viajes
  projection$Proj_Total_Cantidad[i] <- last_total_cantidad + i * average_growth_viajes$Avg_Growth_Viajes * 1000 # Ajuste por proporción de carga
}

# Calcular el porcentaje de crecimiento mensual para viajes
projection <- projection %>%
  mutate(
    Percent_Growth_Viajes = ((Proj_Total_Viajes - lag(Proj_Total_Viajes)) / lag(Proj_Total_Viajes)) * 100
  )

# Mostrar la tabla de proyección con el porcentaje de crecimiento
print("Tabla de Proyección con Porcentaje de Crecimiento del Número de Viajes:")
print(projection)

# Graficar la proyección del número de viajes
ggplot() +
  geom_line(data = monthly_summary, aes(x = as.numeric(MES), y = Total_Viajes), color = "blue", linetype = "dashed") +
  geom_line(data = projection, aes(x = as.numeric(MES), y = Proj_Total_Viajes), color = "red") +
  geom_point(data = monthly_summary, aes(x = as.numeric(MES), y = Total_Viajes), color = "blue") +
  geom_point(data = projection, aes(x = as.numeric(MES), y = Proj_Total_Viajes), color = "red") +
  scale_x_continuous(breaks = seq(1, 36, 1), labels = sprintf("%02d", seq(1, 36, 1))) +
  labs(title = "Proyección del Número de Viajes para los Próximos 24 Meses", x = "Mes", y = "Número de Viajes") +
  theme_minimal()





# Agrupar por mes y calcular la cantidad total de carga y el número de viajes
monthly_summary <- data %>%
  group_by(MES) %>%
  summarise(
    Total_Cantidad = sum(CANTIDAD, na.rm = TRUE),
    Total_Viajes = n(),
    .groups = 'drop'
  ) %>%
  arrange(MES)

# Calcular el crecimiento mensual en carga y viajes en términos absolutos
monthly_summary <- monthly_summary %>%
  mutate(
    Growth_Cantidad = Total_Cantidad - lag(Total_Cantidad, default = first(Total_Cantidad)),
    Growth_Viajes = Total_Viajes - lag(Total_Viajes, default = first(Total_Viajes))
  )

# Calcular el promedio de crecimiento mensual en el número de viajes
average_growth_viajes <- monthly_summary %>%
  summarise(
    Avg_Growth_Viajes = mean(Growth_Viajes, na.rm = TRUE)
  )

# Inicializar valores para proyección
last_month <- max(monthly_summary$MES)
last_total_viajes <- tail(monthly_summary$Total_Viajes, 1)
last_total_cantidad <- tail(monthly_summary$Total_Cantidad, 1)

# Crear un dataframe para las proyecciones
projection <- data.frame(
  MES = sprintf("%02d", seq(as.numeric(last_month) + 1, as.numeric(last_month) + 24)),
  Proj_Total_Viajes = NA,
  Proj_Total_Cantidad = NA
)

# Realizar la proyección para los próximos 24 meses
for (i in 1:nrow(projection)) {
  projection$Proj_Total_Viajes[i] <- last_total_viajes + i * average_growth_viajes$Avg_Growth_Viajes
  projection$Proj_Total_Cantidad[i] <- last_total_cantidad + i * average_growth_viajes$Avg_Growth_Viajes * 1000 # Ajuste por proporción de carga
}

# Mostrar la tabla de proyección
print("Tabla de Proyección de Carga y Viajes:")
print(projection)

# Determinar la capacidad promedio de carga de cada tipo de unidad
unidad_summary <- data %>%
  group_by(UNIDAD) %>%
  summarise(
    Capacidad_Carga = mean(CANTIDAD, na.rm = TRUE),  # Promedio de carga manejada por cada tipo de vehículo
    .groups = 'drop'
  )

# Calcular la carga adicional proyectada
last_total_cantidad_proyectada <- tail(projection$Proj_Total_Cantidad, 1)

# Crear una tabla para la demanda adicional por tipo de unidad
demanda_adicional <- unidad_summary %>%
  mutate(
    Carga_Adicional = last_total_cantidad_proyectada / Capacidad_Carga - 1,  # Calcula la carga adicional basada en la proyección
    Vehiculos_Adicionales = ceiling(Carga_Adicional)
  )

# Mostrar los vehículos adicionales necesarios
print("Número de Vehículos Adicionales Necesarios por Tipo:")
print(demanda_adicional)

# Visualización de la demanda adicional por tipo de unidad
ggplot(demanda_adicional, aes(x = reorder(UNIDAD, Vehiculos_Adicionales), y = Vehiculos_Adicionales, fill = UNIDAD)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Número de Vehículos Adicionales por Tipo", x = "Tipo de Unidad", y = "Número de Vehículos Adicionales")
