library(raster)
library(sf)
library(dplyr)

# Microclima ----
# Establece la ruta a la carpeta que contiene los archivos raster
raster_folder <- "B:/A_ALBERT/CLIMA_DOWNSCALING/MICROCLIMA/TMAX"

# Lee los puntos desde un archivo, puede ser un shapefile, csv, etc.
# Aquí supongo que tienes un shapefile de puntos
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/random_points_10km.shp")

# Convierte los puntos a un objeto SpatialPoints
puntos_sp <- as(puntos, "Spatial")

# Lista de archivos raster en la carpeta
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

# Crear una tabla para almacenar los resultados
resultados <- data.frame(matrix(ncol = 103, nrow = nrow(puntos)))
colnames(resultados) <- c("ID", "X", "Y", basename(raster_files))

# Agregar las coordenadas de los puntos a la tabla
resultados$ID <- 1:nrow(puntos)
resultados$X <- st_coordinates(puntos)[,1]
resultados$Y <- st_coordinates(puntos)[,2]

# Iterar sobre los archivos raster y extraer los valores para cada punto
for (i in 1:length(raster_files)) {
  raster_layer <- raster(raster_files[i])
  valores <- extract(raster_layer, puntos_sp)
  resultados[, i + 3] <- valores
}

resultados$TMAX <- rowMeans(resultados[, 4:(3 + length(raster_files))], na.rm = TRUE)
microclima <- resultados[,c(1,2,3,104)]

# Machispline ----

raster_folder <- "B:/A_ALBERT/CLIMA_DOWNSCALING/RESULTADOS_MACHSPLINE/tmax_down"

# Lee los puntos desde un archivo, puede ser un shapefile, csv, etc.
# Aquí supongo que tienes un shapefile de puntos
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/random_points_10km.shp")

# Convierte los puntos a un objeto SpatialPoints
puntos_sp <- as(puntos, "Spatial")

# Lista de archivos raster en la carpeta
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

# Crear una tabla para almacenar los resultados
resultados <- data.frame(matrix(ncol = length(raster_files) + 3, nrow = nrow(puntos)))
colnames(resultados) <- c("ID", "X", "Y", basename(raster_files))

# Agregar las coordenadas de los puntos a la tabla
resultados$ID <- 1:nrow(puntos)
resultados$X <- st_coordinates(puntos)[,1]
resultados$Y <- st_coordinates(puntos)[,2]

# Iterar sobre los archivos raster y extraer los valores para cada punto
for (i in 1:length(raster_files)) {
  raster_layer <- raster(raster_files[i])
  valores <- extract(raster_layer, puntos_sp)
  resultados[, i + 3] <- valores
}

resultados$TMAX <- rowMeans(resultados[, 4:(3 + length(raster_files))], na.rm = TRUE)
all <-  cbind(resultados, microclima)

all <- all[,c(1:6,10)]

colnames(all) <- c("ID", "X", "Y", "ALT_SLOPE_ASPECT_GEOMORPHONS_TWI", "ALT_SLOPE_ASPECT_TWI", "ALT_SLOPE_TWI", "MICROCLIMA")
library(ggplot2)
library(tidyr)
all_long <- all %>%
  select(ID, ALT_SLOPE_ASPECT_GEOMORPHONS_TWI, ALT_SLOPE_ASPECT_TWI, ALT_SLOPE_TWI, MICROCLIMA) %>%
  gather(key = "Variable", value = "Valor", -ID)

# Crear el gráfico
ggplot(all_long, aes(x = ID, y = Valor, color = Variable)) +
  geom_line() +
  labs(title = "Comparación de Valores para 100 Puntos",
       x = "ID del Punto",
       y = "Valor del Raster",
       color = "Variable") +
  theme_minimal()

cor_resultados <- cor(all[, c("ALT_SLOPE_ASPECT_GEOMORPHONS_TWI", "ALT_SLOPE_ASPECT_TWI", "ALT_SLOPE_TWI", "MICROCLIMA", "TMAX")], use = "complete.obs")
print(cor_resultados)

library(tidyr)
library(dplyr)

# Transformación al formato largo
resultados_long <- all %>%
  select(ID, ALT_SLOPE_ASPECT_GEOMORPHONS_TWI, ALT_SLOPE_ASPECT_TWI, ALT_SLOPE_TWI, MICROCLIMA) %>%
  pivot_longer(cols = -ID, names_to = "Variable", values_to = "Valor")

# Conversión de 'Variable' a factor
resultados_long$Variable <- as.factor(resultados_long$Variable)

# ANOVA de medidas repetidas
anova_result <- aov(Valor ~ Variable + Error(ID/Variable), data = resultados_long)
summary(anova_result)

library(emmeans)

# ANOVA de medidas repetidas
anova_result <- aov(Valor ~ Variable + Error(ID/Variable), data = resultados_long)

# Obtener las medias marginales estimadas
emmeans_result <- emmeans(anova_result, ~ Variable)

# Realizar la prueba post-hoc de Tukey
tukey_result <- pairs(emmeans_result, adjust = "tukey")

# Mostrar los resultados
summary(tukey_result)


library(easyclimate)
coords <- data.frame(lon = -5.36, lat = 37.40)
coords <- all[,c(2,3)]
colnames(coords) <- c("lon", "lat")


tmax <- get_daily_climate(coords, 
                          period = "2012-08-01:2012-08-31", 
                          climatic_var = "Tmax",
                          version = 4) # default
tmax_summary <- tmax %>%
  group_by(ID_coords) %>%  # Agrupamos por la columna 'ID_coords'
  summarise(TMAX = mean(Tmax, na.rm = TRUE)) 
all <- cbind(all, tmax_summary)
all <- all[,-8]

library(climaemet)
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYXJpb19taW5nYXJyb0Bob3RtYWlsLmNvbSIsImp0aSI6IjUyNDAzNzBiLTU4ZWUtNDJmZC1iM2MxLTZjOGJiZDkxN2MyMyIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNTYwNDM5OTU4LCJ1c2VySWQiOiI1MjQwMzcwYi01OGVlLTQyZmQtYjNjMS02YzhiYmQ5MTdjMjMiLCJyb2xlIjoiIn0.AjAxutpJ7B5IdKrHluzX0mMp8VKpbIMIPOUsdzsrRTc", install = TRUE,overwrite=TRUE)
all_stations <- aemet_daily_clim(
  start = "2012-08-01", end = "2012-08-31",
  return_sf = TRUE
)
st_write(all_stations, "B:/A_ALBERT/CLIMA_DOWNSCALING/ESTACIONES_AEMET/estaciones_amet_8_2012.shp")
