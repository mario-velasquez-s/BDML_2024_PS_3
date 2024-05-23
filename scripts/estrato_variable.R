# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez
# Problem Set 3
# 
# Data recovered form:
# https://datosabiertos.bogota.gov.co/dataset/estrato-socioeconomico-bogota-d-c
# https://www.ideca.gov.co/recursos/mapas/lote

<<<<<<< HEAD
# Instalar librerías necesarias si no las tienes

#install.packages("sf")
#install.packages("dplyr")
#install.packages("readr")

# Cargar librerías
library(sf)
library(dplyr)
library(readr)

# Leer el archivo GeoJSON
lotes <- st_read("data/lote.geojson")

# Leer el archivo CSV
estratos <- read_delim("data/esoc.csv", delim = ";")

# Asegurarte de que los nombres de los campos coinciden
names(estratos)[names(estratos) == "ESoCLote"] <- "LOTCODIGO"

# Unir las bases de datos
lotes_estratos <- lotes %>%
  left_join(estratos, by = "LOTCODIGO")

# Asegurar que lotes_estratos está en el CRS correcto
lotes_estratos <- st_transform(lotes_estratos, crs = 4326)

# Cargar los datos de train y test como objetos sf
test <- test %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

train <- train %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Función para encontrar el estrato más cercano para cada propiedad
encontrar_estrato <- function(data, lotes_estratos) {
  nearest_lote <- st_nearest_feature(data, lotes_estratos)
  estrato_value <- lotes_estratos$ESoEstrato[nearest_lote]
  return(estrato_value)
}

# Agregar el estrato más cercano a train y test
train$estrato <- encontrar_estrato(train, lotes_estratos)
test$estrato <- encontrar_estrato(test, lotes_estratos)

# Guardar los datasets actualizados
st_write(test, "data/test_shp_with_estratos.shp")
st_write(train, "data/train_shp_with_estratos.shp")

st_write(test, "data/test_json_with_estratos.geojson")
st_write(train, "data/train_json_with_estratos.geojson")
=======
>>>>>>> 43ae786 (Estrato v1)
