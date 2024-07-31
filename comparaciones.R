library(raster)
library(sf)
library(dplyr)
library(easyclimate)
library(climaemet)
library(ggplot2)
library(tidyr)
library(dplyr)
library(emmeans)

# TMAX ----
# Microclima 1 ----
# Sin efectos de la costa
# Establece la ruta a la carpeta que contiene los archivos raster
raster_folder <- "B:/A_ALBERT/CLIMA_DOWNSCALING/MICROCLIMA/NCOASTAL_H7/TMAX/"

# Lee los puntos desde un archivo
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")

# Convierte los puntos a un objeto SpatialPoints
puntos_sp <- as(puntos, "Spatial")

# Lista de archivos raster en la carpeta
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

# Crear una tabla para almacenar los resultados
resultados <- data.frame(matrix(ncol = 103, nrow = nrow(puntos)))
colnames(resultados) <- c("ID", "X", "Y", basename(raster_files))

# Agregar las coordenadas de los puntos a la tabla
resultados$ID <- puntos$indictv
resultados$X <- st_coordinates(puntos)[,1]
resultados$Y <- st_coordinates(puntos)[,2]

# Iterar sobre los archivos raster y extraer los valores para cada punto
for (i in 1:length(raster_files)) {
  raster_layer <- raster(raster_files[i])
  valores <- terra::extract(raster_layer, puntos_sp)
  resultados[, i + 3] <- valores
}

resultados$TMAX <- rowMeans(resultados[, 4:(3 + length(raster_files))], na.rm = TRUE)

microclima1 <- resultados[,c(1,2,3,104)]
colnames(microclima1) <- c("ID","X", "Y", "Microclima_1")

# Eliminar objetos 
rm(raster_folder, puntos, puntos_sp, raster_files, raster_layer, valores, resultados, i)

# Microclima 2 ----
# 
# raster_folder <- "B:/A_ALBERT/CLIMA_DOWNSCALING/MICROCLIMA/COASTAL_H7/TMAX/"
# puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
# puntos_sp <- as(puntos, "Spatial")
# raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)
# resultados <- data.frame(matrix(ncol = 103, nrow = nrow(puntos)))
# colnames(resultados) <- c("ID", "X", "Y", basename(raster_files))
# 
# resultados$ID <- puntos$indictv
# resultados$X <- st_coordinates(puntos)[,1]
# resultados$Y <- st_coordinates(puntos)[,2]
# 
# for (i in 1:length(raster_files)) {
#   raster_layer <- raster(raster_files[i])
#   valores <- terra::extract(raster_layer, puntos_sp)
#   resultados[, i + 3] <- valores
# }
# 
# resultados$TMAX <- rowMeans(resultados[, 4:(3 + length(raster_files))], na.rm = TRUE)
# microclima2 <- resultados[,c(1,2,3,104)]


# Machispline ----
raster_folder <- "B:/A_ALBERT/CLIMA_DOWNSCALING/RESULTADOS_MACHSPLINE/tmax_down"
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
puntos_sp <- as(puntos, "Spatial")
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)
resultados <- data.frame(matrix(ncol = length(raster_files) + 3, nrow = nrow(puntos)))
colnames(resultados) <- c("ID", "X", "Y", basename(raster_files))
resultados$ID <- puntos$indictv
resultados$X <- st_coordinates(puntos)[,1]
resultados$Y <- st_coordinates(puntos)[,2]
for (i in 1:length(raster_files)) {
  raster_layer <- raster(raster_files[i])
  valores <- terra::extract(raster_layer, puntos_sp)
  resultados[, i + 3] <- valores
}

resultados$MEAN_MCH <- rowMeans(resultados[, 4:(3 + length(raster_files))], na.rm = TRUE)
machispline <- resultados
rm(raster_folder, puntos, puntos_sp, raster_files, raster_layer, valores, resultados, i)

# Easyclimate ----
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
coords <- as.data.frame(puntos[,c(26,27)])
coords <- coords[,1:2]
colnames(coords) <- c("lon", "lat")


easyclimate <- get_daily_climate(coords, 
                          period = "2012-08-01:2012-08-31", 
                          climatic_var = "Tmax",
                          version = 4)
easyclimate <- easyclimate %>%
  group_by(ID_coords) %>% 
  summarise(TMAX = mean(Tmax, na.rm = TRUE)) 
puntos <- as.data.frame(puntos)
easyclimate <- cbind(puntos[,c(2,26,27)], easyclimate)
easyclimate <- easyclimate[,-4]
colnames(easyclimate) <- c("ID","X", "Y", "Easyclimate")
rm(puntos,coords)

# AEMET ----
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYXJpb19taW5nYXJyb0Bob3RtYWlsLmNvbSIsImp0aSI6IjUyNDAzNzBiLTU4ZWUtNDJmZC1iM2MxLTZjOGJiZDkxN2MyMyIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNTYwNDM5OTU4LCJ1c2VySWQiOiI1MjQwMzcwYi01OGVlLTQyZmQtYjNjMS02YzhiYmQ5MTdjMjMiLCJyb2xlIjoiIn0.AjAxutpJ7B5IdKrHluzX0mMp8VKpbIMIPOUsdzsrRTc", install = TRUE,overwrite=TRUE)

AEMET_stations <- aemet_daily_clim(
  start = "2012-08-01", end = "2012-08-31",
  return_sf = FALSE
)

AEMET <- AEMET_stations %>% 
  group_by(indicativo) %>% 
  summarise(AEMET = mean(tmax, na.rm = TRUE)) 

AEMET <- left_join(puntos, AEMET, by=c("indictv" = "indicativo"))
AEMET <- as.data.frame(AEMET)
AEMET <- AEMET[, c(2,26,27,10)]
AEMET[AEMET == 0] <- NA

colnames(AEMET) <- c("ID","X", "Y", "AEMET")

rm(AEMET_stations, puntos)

# Terraclimate ----
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
puntos_sp <- as(puntos, "Spatial")

terraclimate <- raster::stack("B:/A_ALBERT/CLIMA_DOWNSCALING/TERRACLIMATE/TerraClimate_tmax_2012.nc")
terraclimate <- terraclimate[[8]]

names(terraclimate) <- paste0("TERRACLIMATE")
terraclimate <- terra::extract(terraclimate, puntos_sp)
terraclimate <- cbind(puntos, terraclimate)
terraclimate <- as.data.frame(terraclimate)
terraclimate <- terraclimate[, c(2,26:28)]
colnames(terraclimate) <- c("ID","X", "Y", "Terraclimate")

rm(puntos, puntos_sp)


# Unificar
tmax <-  cbind(machispline, easyclimate, AEMET, terraclimate, microclima1 )

tmax <- tmax[,c(1:7,11,15,19,23)]

colnames(all) <- c("ID", "X", "Y", "Machispline_A_S_A_G_T", 
                   "Machispline_A_S_A_T", "Machispline_A_S_T", 
                   "Mean_Machispline", "Easyclimate", 
                   "AEMET","Terraclimate", "Microclima_NCE")


tmax <- mutate(tmax, "N" = seq(1, 96,1))
#write.csv2(tmax, "B:/A_ALBERT/ALL_TMAX_8_2012.csv")

# TMIN ----
# Microclima 1 ----
# Sin efectos de la costa
# Establece la ruta a la carpeta que contiene los archivos raster
raster_folder <- "B:/A_ALBERT/CLIMA_DOWNSCALING/MICROCLIMA/NCOASTAL_H7/TMIN/"

# Lee los puntos desde un archivo
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")

# Convierte los puntos a un objeto SpatialPoints
puntos_sp <- as(puntos, "Spatial")

# Lista de archivos raster en la carpeta
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

# Crear una tabla para almacenar los resultados
resultados <- data.frame(matrix(ncol = 103, nrow = nrow(puntos)))
colnames(resultados) <- c("ID", "X", "Y", basename(raster_files))

# Agregar las coordenadas de los puntos a la tabla
resultados$ID <- puntos$indictv
resultados$X <- st_coordinates(puntos)[,1]
resultados$Y <- st_coordinates(puntos)[,2]

# Iterar sobre los archivos raster y extraer los valores para cada punto
for (i in 1:length(raster_files)) {
  raster_layer <- raster(raster_files[i])
  valores <- terra::extract(raster_layer, puntos_sp)
  resultados[, i + 3] <- valores
}

resultados$TMAX <- rowMeans(resultados[, 4:(3 + length(raster_files))], na.rm = TRUE)

microclima1 <- resultados[,c(1,2,3,104)]
colnames(microclima1) <- c("ID","X", "Y", "Microclima_1")

# Eliminar objetos 
rm(raster_folder, puntos, puntos_sp, raster_files, raster_layer, valores, resultados, i)

# Microclima 2 ----

# raster_folder <- "B:/A_ALBERT/CLIMA_DOWNSCALING/MICROCLIMA/COASTAL_H7/TMAX/"
# puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
# puntos_sp <- as(puntos, "Spatial")
# raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)
# resultados <- data.frame(matrix(ncol = 103, nrow = nrow(puntos)))
# colnames(resultados) <- c("ID", "X", "Y", basename(raster_files))
# 
# resultados$ID <- puntos$indictv
# resultados$X <- st_coordinates(puntos)[,1]
# resultados$Y <- st_coordinates(puntos)[,2]
# 
# for (i in 1:length(raster_files)) {
#   raster_layer <- raster(raster_files[i])
#   valores <- terra::extract(raster_layer, puntos_sp)
#   resultados[, i + 3] <- valores
# }
# 
# resultados$TMAX <- rowMeans(resultados[, 4:(3 + length(raster_files))], na.rm = TRUE)
# microclima2 <- resultados[,c(1,2,3,104)]


# Machispline ----
raster_folder <- "B:/A_ALBERT/CLIMA_DOWNSCALING/RESULTADOS_MACHSPLINE/tmin_down"
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
puntos_sp <- as(puntos, "Spatial")
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)
resultados <- data.frame(matrix(ncol = length(raster_files) + 3, nrow = nrow(puntos)))
colnames(resultados) <- c("ID", "X", "Y", basename(raster_files))
resultados$ID <- puntos$indictv
resultados$X <- st_coordinates(puntos)[,1]
resultados$Y <- st_coordinates(puntos)[,2]
for (i in 1:length(raster_files)) {
  raster_layer <- raster(raster_files[i])
  valores <- terra::extract(raster_layer, puntos_sp)
  resultados[, i + 3] <- valores
}

resultados$MEAN_MCH <- rowMeans(resultados[, 4:(3 + length(raster_files))], na.rm = TRUE)
machispline <- resultados
rm(raster_folder, puntos, puntos_sp, raster_files, raster_layer, valores, resultados, i)

# Easyclimate ----
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
coords <- as.data.frame(puntos[,c(26,27)])
coords <- coords[,1:2]
colnames(coords) <- c("lon", "lat")


easyclimate <- get_daily_climate(coords, 
                                 period = "2012-08-01:2012-08-31", 
                                 climatic_var = "Tmin",
                                 version = 4)
easyclimate <- easyclimate %>%
  group_by(ID_coords) %>% 
  summarise(TMIN = mean(Tmin, na.rm = TRUE))

puntos <- as.data.frame(puntos)
easyclimate <- cbind(puntos[,c(2,26,27)], easyclimate)
easyclimate <- easyclimate[,-4]
colnames(easyclimate) <- c("ID","X", "Y", "Easyclimate")
rm(puntos,coords)

# AEMET ----
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYXJpb19taW5nYXJyb0Bob3RtYWlsLmNvbSIsImp0aSI6IjUyNDAzNzBiLTU4ZWUtNDJmZC1iM2MxLTZjOGJiZDkxN2MyMyIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNTYwNDM5OTU4LCJ1c2VySWQiOiI1MjQwMzcwYi01OGVlLTQyZmQtYjNjMS02YzhiYmQ5MTdjMjMiLCJyb2xlIjoiIn0.AjAxutpJ7B5IdKrHluzX0mMp8VKpbIMIPOUsdzsrRTc", install = TRUE,overwrite=TRUE)

AEMET_stations <- aemet_daily_clim(
  start = "2012-08-01", end = "2012-08-31",
  return_sf = FALSE
)

AEMET <- AEMET_stations %>% 
  group_by(indicativo) %>% 
  summarise(AEMET = mean(tmin, na.rm = TRUE)) 

puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
AEMET <- left_join(puntos, AEMET, by=c("indictv" = "indicativo"))
AEMET <- as.data.frame(AEMET)
AEMET <- AEMET[, c(2,26,27,8)]
AEMET[AEMET == 0] <- NA

colnames(AEMET) <- c("ID","X", "Y", "AEMET")

rm(AEMET_stations, puntos)

# Terraclimate ----
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")
puntos_sp <- as(puntos, "Spatial")

terraclimate <- raster::stack("B:/A_ALBERT/CLIMA_DOWNSCALING/TERRACLIMATE/TerraClimate_tmin_2012.nc")
terraclimate <- terraclimate[[8]]

names(terraclimate) <- paste0("TERRACLIMATE")
terraclimate <- terra::extract(terraclimate, puntos_sp)
terraclimate <- cbind(puntos, terraclimate)
terraclimate <- as.data.frame(terraclimate)
terraclimate <- terraclimate[, c(2,26:28)]
colnames(terraclimate) <- c("ID","X", "Y", "Terraclimate")

rm(puntos, puntos_sp)


# Unificar
tmin <-  cbind(machispline, easyclimate, AEMET, terraclimate, microclima1 )

tmin <- tmin[,c(1:7,11,15,19,23)]

colnames(tmin) <- c("ID", "X", "Y", "Machispline_A_S_A_G_T", 
                   "Machispline_A_S_A_T", "Machispline_A_S_T", 
                   "Mean_Machispline", "Easyclimate", 
                   "AEMET","Terraclimate", "Microclima_NCE")


tmin <- mutate(tmin, "N" = seq(1, 96,1))
#write.csv2(tmin, "B:/A_ALBERT/ALL_TMIN_8_2012.csv")
all <- tmin

# Resultados ----
all_long <- all %>%
  dplyr::select(N, Machispline_A_S_A_G_T, 
                Machispline_A_S_A_T, Machispline_A_S_T, 
                Mean_Machispline, Easyclimate, 
                AEMET,Terraclimate, Microclima_NCE) %>%
  tidyr::gather(key = "Variable", value = "Valor", -N)

# Graficos----
# Grafico TMAX por estacion
ggplot(all_long, aes(x = N, y = Valor, color = Variable)) +
  geom_line() +
  geom_smooth(method = lm, se = F)+
  labs(x = "ID Estación",
       y = "Tmin (ºC)",
       color = "Variable") +
  theme(legend.position = "top",
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray80"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray80"))+
  scale_color_brewer(palette = "Spectral")

# Grafico elevación por estacion
puntos <- st_read("B:/A_ALBERT/CLIMA_DOWNSCALING/estaciones_AE.shp")  
ggplot(puntos, aes(x=indictv, y = altitud))+
  geom_point()+
  scale_color_viridis_b()+
  labs(x = "ID Estación",
       y = "Elevacion",
       color = "Variable") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray80"), 
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray80"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  
  
# Estadísticos ----
## Correlación ----
cor_resultados <- cor(all[, c("Machispline_A_S_A_G_T", 
                              "Machispline_A_S_A_T", "Machispline_A_S_T", 
                              "Mean_Machispline", "Easyclimate", 
                              "AEMET","Terraclimate", "Microclima_NCE")], use = "complete.obs")
print(cor_resultados)


## Anova ----
resultados_long <- all %>%
  dplyr::select(N,Machispline_A_S_A_G_T, 
                Machispline_A_S_A_T, Machispline_A_S_T, 
                Mean_Machispline, Easyclimate, 
                AEMET,Terraclimate, Microclima_NCE) %>%
  pivot_longer(cols = -N, names_to = "Variable", values_to = "Valor")

resultados_long$Variable <- as.factor(resultados_long$Variable)

# ANOVA de medidas repetidas
anova_result <- aov(Valor ~ Variable + Error(N/Variable), data = resultados_long)
summary(anova_result)


# Medias marginales estimadas
emmeans_result <- emmeans(anova_result, ~ Variable)

# Test post-hoc Tukey
tukey_result <- pairs(emmeans_result, adjust = "tukey")

# Mostrar los resultados
summary(tukey_result)



write.csv2(ss, "B:/A_ALBERT/tukey.csv")

