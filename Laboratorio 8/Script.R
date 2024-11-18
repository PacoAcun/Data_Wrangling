# Librerias
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

data <- read_csv("Laboratorio 8/c1.csv")
View(data)

# Limpieza de Datos

# Convertir la columna 'Fecha' al formato de fecha correcto
data$Fecha <- as.Date(data$Fecha, format = "%d-%m-%y")
head(data$Fecha)

# Camion 

data$Camion_5 <- gsub("Q-", "", data$Camion_5)
data$Camion_5 <- gsub("Q", "", data$Camion_5)
data$Camion_5[data$Camion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$Camion_5 <- as.numeric(data$Camion_5)
class(data$Camion_5)
colnames(data)[colnames(data) == "Camion_5"] <- "Camion"

# Pickup

data$Pickup <- gsub("Q-", "", data$Pickup)
data$Pickup <- gsub("Q", "", data$Pickup)
data$Pickup[data$Pickup == ""] <- NA  # Asigna NA a valores vacíos
class(data$Pickup)

# Moto

data$Moto <- gsub("Q-", "", data$Moto)
data$Moto <- gsub("Q", "", data$Moto)
data$Moto[data$Moto == ""] <- NA  # Asigna NA a valores vacíos
data$Moto <- as.numeric(data$Moto)
class(data$Moto)

# Factura

data$factura <- gsub("Q-", "", data$factura)
data$factura <- gsub("Q", "", data$factura)
data$factura[data$factura == ""] <- NA  # Asigna NA a valores vacíos
data$factura <- as.numeric(data$factura)
class(data$factura)

# directoCamion_5

data$directoCamion_5 <- gsub("Q-", "", data$directoCamion_5)
data$directoCamion_5 <- gsub("Q", "", data$directoCamion_5)
data$directoCamion_5[data$directoCamion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$directoCamion_5 <- as.numeric(data$directoCamion_5)
class(data$directoCamion_5)
colnames(data)[colnames(data) == "directoCamion_5"] <- "directoCamion"

# directoPickup

data$directoPickup <- gsub("Q-", "", data$directoPickup)
data$directoPickup <- gsub("Q", "", data$directoPickup)
data$directoPickup[data$directoPickup == ""] <- NA  # Asigna NA a valores vacíos
data$directoPickup <- as.numeric(data$directoPickup)
class(data$directoPickup)

# directoMoto

data$directoMoto<- gsub("Q-", "", data$directoMoto)
data$directoMoto <- gsub("Q", "", data$directoMoto)
data$directoMoto[data$directoMoto == ""] <- NA  # Asigna NA a valores vacíos
data$directoMoto <- as.numeric(data$directoMoto)
class(data$directoMoto)

# fijoCamion_5

data$fijoCamion_5 <- gsub("Q-", "", data$fijoCamion_5)
data$fijoCamion_5 <- gsub("Q", "", data$fijoCamion_5)
data$fijoCamion_5[data$fijoCamion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$fijoCamion_5 <- as.numeric(data$fijoCamion_5)
class(data$fijoCamion_5)
colnames(data)[colnames(data) == "fijoCamion_5"] <- "fijoCamion"

# fijoPickup

data$fijoPickup<- gsub("Q-", "", data$fijoPickup)
data$fijoPickup <- gsub("Q", "", data$fijoPickup)
data$fijoPickup[data$fijoPickup == ""] <- NA  # Asigna NA a valores vacíos
data$fijoPickup <- as.numeric(data$fijoPickup)
class(data$fijoPickup)

# fijoMoto

data$fijoMoto<- gsub("Q-", "", data$fijoMoto)
data$fijoMoto <- gsub("Q", "", data$fijoMoto)
data$fijoMoto[data$fijoMoto == ""] <- NA  # Asigna NA a valores vacíos
data$fijoMoto <- as.numeric(data$fijoMoto)
class(data$fijoMoto)

# Conversión de X a Booleanos

# 5-30 minutos

data$`5-30` <- gsub("x", "TRUE", data$`5-30`)
data$`5-30`[is.na(data$`5-30`) | data$`5-30` == ""] <- "FALSE"
data$`5-30` <- as.logical(data$`5-30`)
head(data$`5-30`)
class(data$`5-30`)


# 30 - 45 minutos

data$`30-45` <- gsub("x", "TRUE", data$`30-45`)
data$`30-45`[is.na(data$`30-45`) | data$`30-45` == ""] <- "FALSE"
data$`30-45` <- as.logical(data$`30-45`)
head(data$`30-45`)
class(data$`30-45`)

# 45 - 75 minutos

data$`45-75` <- gsub("x", "TRUE", data$`45-75`)
data$`45-75`[is.na(data$`45-75`) | data$`45-75` == ""] <- "FALSE"
data$`45-75` <- as.logical(data$`45-75`)
head(data$`45-75`)
class(data$`45-75`)

# 75 - 120 minutos

data$`75-120` <- gsub("x", "TRUE", data$`75-120`)
data$`75-120`[is.na(data$`75-120`) | data$`75-120` == ""] <- "FALSE"
data$`75-120` <- as.logical(data$`75-120`)
head(data$`75-120`)
class(data$`75-120`)

# Más de 120 minutos

data$`120+` <- gsub("x", "TRUE", data$`120+`)
data$`120+`[is.na(data$`120+`) | data$`120+` == ""] <- "FALSE"
data$`120+` <- as.logical(data$`120+`)
head(data$`120+`)
class(data$`120+`)

# Eliminar columnas que no sirven de nada 
data <- data[, !names(data) %in% c("...23", "...24", "...25", "...26", "...27", "...28")]
head(data)



# Convertir columnas a numéricas y luego calcular los valores de Costo_Total y Utilidad
data <- data %>%
  mutate(
    Camion = as.numeric(Camion),
    Pickup = as.numeric(Pickup),
    Moto = as.numeric(Moto)
  ) %>%
  mutate(
    Costo_Total = rowSums(select(., c(Camion, Pickup, Moto)), na.rm = TRUE),  # Sumar costos de cada vehículo
    Utilidad = factura - Costo_Total                                          # Calcular utilidad
  )

# Calcular utilidad por viaje
data <- data %>%
  mutate(
    Costo_Total = rowSums(select(., c(Camion, Pickup, Moto)), na.rm = TRUE),  # Sumar costos de cada vehículo
    Utilidad = factura - Costo_Total                                          # Calcular utilidad
  )

# Unificar distancias en una sola columna
data <- data %>%
  mutate(
    Distancia = case_when(
      `5-30` == TRUE ~ "5-30",
      `30-45` == TRUE ~ "30-45",
      `45-75` == TRUE ~ "45-75",
      `75-120` == TRUE ~ "75-120",
      `120+` == TRUE ~ "120+",
      TRUE ~ "NA"   # En caso de que no haya ninguna distancia marcada
    )
  ) %>%
  select(-c(`5-30`, `30-45`, `45-75`, `75-120`, `120+`))  # Eliminar las columnas originales de distancia

# Análisis 80-20 de facturación
facturacion_por_poste <- data %>%
  group_by(ID) %>%
  summarise(
    Facturacion_Total = sum(factura, na.rm = TRUE),
    Costo_Total = sum(Costo_Total, na.rm = TRUE),
    Utilidad_Total = sum(Utilidad, na.rm = TRUE)
  ) %>%
  arrange(desc(Facturacion_Total))

# Calcular el porcentaje acumulativo de facturación (para encontrar el 80-20)
facturacion_por_poste <- facturacion_por_poste %>%
  mutate(
    Porcentaje_Acumulado = cumsum(Facturacion_Total) / sum(Facturacion_Total) * 100
  )

# Evaluar el costo promedio por unidad de altura
costo_por_altura <- data %>%
  group_by(height) %>%
  summarise(
    Costo_Medio = mean(Costo_Total, na.rm = TRUE),
    Utilidad_Media = mean(Utilidad, na.rm = TRUE)
  )

# Calcular utilidad total de 2017 y luego estimar utilidad de 2018
utilidad_2017 <- sum(data$Utilidad, na.rm = TRUE)  # Sumar utilidad total para 2017
utilidad_2018 <- utilidad_2017 * 0.75  # Reducir un 25% para estimar 2018
utilidad_objetivo_2019 <- utilidad_2018 * 1.1  # Aumentar un 10% para la meta de 2019

# Contar viajes por tipo de vehículo
data <- data %>%
  mutate(
    Camion_Trips = ifelse(!is.na(Camion), 1, 0),
    Pickup_Trips = ifelse(!is.na(Pickup), 1, 0),
    Moto_Trips = ifelse(!is.na(Moto), 1, 0)
  )

# Análisis de costos por tipo de vehículo
costos_por_vehiculo <- data %>%
  summarise(
    Costo_Camion = sum(Camion, na.rm = TRUE),
    Costo_Pickup = sum(Pickup, na.rm = TRUE),
    Costo_Moto = sum(Moto, na.rm = TRUE)
  )


# Calcular el costo promedio por categoría de distancia
costo_por_distancia <- data %>%
  group_by(Distancia) %>%
  summarise(
    Costo_Promedio = mean(Costo_Total, na.rm = TRUE)
  )

# Visualización de costo promedio por distancia
ggplot(costo_por_distancia, aes(x = Distancia, y = Costo_Promedio)) +
  geom_bar(stat = "identity") +
  labs(title = "Costo Promedio por Categoría de Distancia")


# Identificar postes con alto costo y baja utilidad
postes_alto_costo <- facturacion_por_poste %>%
  filter(Utilidad_Total < 0 | Costo_Total > quantile(Costo_Total, 0.8))

# Comparar tarifas de facturación de 2017 y 2018
ggplot(data, aes(x = as.factor(year(Fecha)), y = factura)) +
  geom_boxplot() +
  labs(title = "Comparación de Tarifas de Facturación entre 2017 y 2018")


# Estrategia 1: Aumentar la eficiencia en costos
# Identificar los vehículos con mayor costo y proponer la optimización de su uso
costos_por_vehiculo <- data %>%
  summarise(
    Costo_Camion = sum(Camion, na.rm = TRUE),
    Costo_Pickup = sum(Pickup, na.rm = TRUE),
    Costo_Moto = sum(Moto, na.rm = TRUE)
  )

# Estrategia 2: Ajustar tarifas de facturación
# Comparar costos promedio por tipo de vehículo y sugerir tarifas
ajuste_tarifas <- data %>%
  group_by(ID) %>%
  summarise(
    Costo_Total = sum(Costo_Total, na.rm = TRUE),
    Facturacion_Total = sum(factura, na.rm = TRUE),
    Utilidad_Total = sum(Utilidad, na.rm = TRUE)
  ) %>%
  mutate(
    Tarifa_Sugerida = ifelse(Utilidad_Total < 0, 
                              Facturacion_Total * 1.10,  # Aumentar tarifas en un 10% si la utilidad es negativa
                              Facturacion_Total)
  )

# Estrategia 3: Diversificar servicios
# Evaluar la facturación de cada vehículo para identificar oportunidades de diversificación
diversificacion_servicios <- data %>%
  group_by(Distancia) %>%
  summarise(
    Facturacion_Total = sum(factura, na.rm = TRUE),
    Utilidad_Total = sum(Utilidad, na.rm = TRUE)
  )

# Mostrar resultados de las estrategias
print("Costos por vehículo:")
print(costos_por_vehiculo)

print("Ajuste de tarifas sugerido:")
print(ajuste_tarifas)

print("Oportunidades de diversificación de servicios:")
print(diversificacion_servicios)

# Resultados generales de utilidad
print(paste("Utilidad total 2017:", utilidad_2017))
print(paste("Estimación de utilidad 2018:", utilidad_2018))
print(paste("Meta de utilidad para 2019:", utilidad_objetivo_2019))


# Agrupación de datos por tipo de servicio (Cod)
servicios_agrupados <- data %>%
  group_by(Cod) %>%
  summarise(
    Facturacion_Total = sum(factura, na.rm = TRUE),
    Costo_Total = sum(Costo_Total, na.rm = TRUE),
    Utilidad_Total = sum(Utilidad, na.rm = TRUE),
    .groups = 'drop'
  )

# Análisis de servicios
servicios_agrupados <- servicios_agrupados %>%
  mutate(
    Margen_Utilidad = Utilidad_Total / Facturacion_Total,
    Ajuste_Tarifa = ifelse(Margen_Utilidad < 0.2, Facturacion_Total * 1.15, Facturacion_Total * 1.05)  # Aumenta tarifas en función del margen
  )

# Revisión de costos
# Reducir costos donde la utilidad es baja
servicios_agrupados <- servicios_agrupados %>%
  mutate(
    Reduccion_Costo = ifelse(Utilidad_Total < 1000, Costo_Total * 0.9, Costo_Total)  # Reduce costos en un 10% donde la utilidad es baja
  )

# Visualización de resultados
print(servicios_agrupados)



# Análisis de eficiencia por tipo de vehículo y distancia
eficiencia_vehiculos <- data %>%
  group_by(Distancia) %>%
  summarise(
    Eficiencia_Camion = mean(Camion/factura, na.rm = TRUE),
    Eficiencia_Pickup = mean(Pickup/factura, na.rm = TRUE),
    Eficiencia_Moto = mean(Moto/factura, na.rm = TRUE)
  )

# Optimización de rutas por zona
analisis_rutas <- data %>%
  group_by(ID, Distancia) %>%
  summarise(
    Frecuencia_Visitas = n(),
    Costo_Promedio = mean(Costo_Total, na.rm = TRUE),
    Utilidad_Promedio = mean(Utilidad, na.rm = TRUE)
  )

# Análisis de rentabilidad por servicio y distancia
rentabilidad_servicios <- data %>%
  group_by(Cod, Distancia) %>%
  summarise(
    Margen_Promedio = mean(Utilidad/factura, na.rm = TRUE),
    Volumen_Servicios = n(),
    Ingreso_Total = sum(factura, na.rm = TRUE)
  ) %>%
  arrange(desc(Margen_Promedio))

# Análisis de oportunidades de nuevos servicios
oportunidades_servicios <- data %>%
  group_by(Cod) %>%
  summarise(
    Frecuencia = n(),
    Margen_Promedio = mean(Utilidad/factura, na.rm = TRUE),
    Crecimiento_Potencial = sum(Utilidad, na.rm = TRUE) * 0.25
  ) %>%
  arrange(desc(Margen_Promedio))




# Análisis de centros de distribución
# Evaluar la necesidad de nuevos centros basado en distancias y costos
analisis_distribucion <- data %>%
  group_by(Distancia) %>%
  summarise(
    Cantidad_Servicios = n(),
    Costo_Promedio = mean(Costo_Total, na.rm = TRUE),
    Utilidad_Promedio = mean(Utilidad, na.rm = TRUE),
    Porcentaje_Servicios = n() / nrow(data) * 100
  ) %>%
  arrange(desc(Cantidad_Servicios))

# Visualizar distribución de servicios por distancia
ggplot(analisis_distribucion, aes(x = Distancia, y = Cantidad_Servicios)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje_Servicios)), 
            vjust = -0.5) +
  labs(title = "Distribución de Servicios por Distancia",
       x = "Rango de Distancia",
       y = "Cantidad de Servicios") +
  theme_minimal()

# Análisis de pérdidas potenciales en mantenimiento/reparación
analisis_perdidas <- data %>%
  group_by(Cod) %>%
  summarise(
    Servicios_Total = n(),
    Costo_Promedio = mean(Costo_Total, na.rm = TRUE),
    Utilidad_Promedio = mean(Utilidad, na.rm = TRUE),
    Servicios_Perdida = sum(Utilidad < 0, na.rm = TRUE),
    Porcentaje_Perdida = (sum(Utilidad < 0, na.rm = TRUE) / n()) * 100
  ) %>%
  arrange(desc(Porcentaje_Perdida))

# Tarifario 2017 por unidad
tarifario_2017 <- data %>%
  group_by(Cod) %>%
  summarise(
    Tarifa_Promedio = mean(factura, na.rm = TRUE),
    Tarifa_Min = min(factura, na.rm = TRUE),
    Tarifa_Max = max(factura, na.rm = TRUE),
    Utilidad_Promedio = mean(Utilidad, na.rm = TRUE),
    Margen_Porcentual = (mean(Utilidad, na.rm = TRUE) / mean(factura, na.rm = TRUE)) * 100
  ) %>%
  arrange(desc(Tarifa_Promedio))

# Análisis de tarifas actuales y números rojos
analisis_tarifas <- data %>%
  group_by(Cod) %>%
  summarise(
    Total_Servicios = n(),
    Servicios_Negativos = sum(Utilidad < 0, na.rm = TRUE),
    Porcentaje_Negativos = (sum(Utilidad < 0, na.rm = TRUE) / n()) * 100,
    Utilidad_Promedio = mean(Utilidad, na.rm = TRUE),
    Margen_Promedio = (mean(Utilidad, na.rm = TRUE) / mean(factura, na.rm = TRUE)) * 100
  ) %>%
  arrange(desc(Porcentaje_Negativos))

# Análisis 80-20 de facturación y mantenimiento
analisis_pareto <- data %>%
  group_by(ID) %>%
  summarise(
    Facturacion_Total = sum(factura, na.rm = TRUE),
    Cantidad_Servicios = n(),
    Costo_Total = sum(Costo_Total, na.rm = TRUE)
  ) %>%
  arrange(desc(Facturacion_Total)) %>%
  mutate(
    Facturacion_Acumulada = cumsum(Facturacion_Total),
    Porcentaje_Acumulado = (Facturacion_Acumulada / sum(Facturacion_Total)) * 100
  )

# Identificar postes que representan el 80% de la facturación
postes_80 <- analisis_pareto %>%
  filter(Porcentaje_Acumulado <= 80)

# Visualizar recorridos más efectivos
recorridos_efectivos <- data %>%
  group_by(Distancia) %>%
  summarise(
    Utilidad_Promedio = mean(Utilidad, na.rm = TRUE),
    Costo_Promedio = mean(Costo_Total, na.rm = TRUE),
    Eficiencia = Utilidad_Promedio / Costo_Promedio,
    Cantidad_Servicios = n()
  ) %>%
  arrange(desc(Eficiencia))

# Visualización de recorridos efectivos
ggplot(recorridos_efectivos, aes(x = Distancia, y = Eficiencia)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = sprintf("%.2f", Eficiencia)), vjust = -0.5) +
  labs(title = "Eficiencia por Rango de Distancia",
       x = "Rango de Distancia",
       y = "Eficiencia (Utilidad/Costo)") +
  theme_minimal()

# Resumen de hallazgos principales
print("=== ANÁLISIS DE CENTROS DE DISTRIBUCIÓN ===")
print(analisis_distribucion)
print("\n=== ANÁLISIS DE PÉRDIDAS POTENCIALES ===")
print(analisis_perdidas)
print("\n=== TARIFARIO 2017 ===")
print(tarifario_2017)
print("\n=== ANÁLISIS DE TARIFAS ACTUALES ===")
print(analisis_tarifas)
print("\n=== POSTES QUE REPRESENTAN 80% DE FACTURACIÓN ===")
print(postes_80)
print("\n=== RECORRIDOS MÁS EFECTIVOS ===")
print(recorridos_efectivos)

# Crear visualizaciones adicionales para presentación
# Gráfico de Pareto
ggplot(analisis_pareto, aes(x = reorder(ID, -Facturacion_Total), y = Porcentaje_Acumulado)) +
  geom_line(group = 1) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
  labs(title = "Análisis de Pareto - Facturación por Poste",
       x = "ID del Poste",
       y = "Porcentaje Acumulado de Facturación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# Resumen de recomendaciones basado en análisis
recomendaciones <- data.frame(
  Aspecto = c("Nuevos Centros", "Prevención Pérdidas", "Ajuste Tarifas", "Optimización Rutas"),
  Recomendación = c(
    ifelse(sum(analisis_distribucion$Porcentaje_Servicios[analisis_distribucion$Distancia %in% c("75-120", "120+")]) > 30,
           "Se recomienda abrir nuevo centro", "No se requieren nuevos centros por ahora"),
    paste("Revisar servicios con", round(mean(analisis_perdidas$Porcentaje_Perdida), 2), "% de pérdidas"),
    ifelse(mean(analisis_tarifas$Margen_Promedio) < 20,
           "Se requiere ajuste de tarifas", "Tarifas actuales son adecuadas"),
    paste("Priorizar rutas", recorridos_efectivos$Distancia[1], "con eficiencia", 
          round(recorridos_efectivos$Eficiencia[1], 2))
  )
)

print("\n=== RECOMENDACIONES FINALES ===")
print(recomendaciones)



# Contar viajes por tipo de vehículo
data <- data %>%
  mutate(
    Camion_Trips = ifelse(!is.na(Camion), 1, 0),
    Pickup_Trips = ifelse(!is.na(Pickup), 1, 0),
    Moto_Trips = ifelse(!is.na(Moto), 1, 0)
  )

# Resumen de viajes por tipo de vehículo
vehicle_counts <- data %>%
  summarize(
    Camion_Count = sum(Camion_Trips),
    Pickup_Count = sum(Pickup_Trips),
    Moto_Count = sum(Moto_Trips)
  )

print(vehicle_counts)