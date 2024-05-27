# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez
# Problem Set 3
# 
# Creation of geographic data using OPEN MAPS
test <- test %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

train <- train %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

bogota<-opq(bbox = getbb("Bogotá Colombia"))

##Restaurantes

restaurants <- bogota %>%
  add_osm_feature(key = 'amenity', value = 'restaurant') %>%
  osmdata_sf()

restaurantes <- restaurants$osm_points

##Parques

parks <- bogota %>%
  add_osm_feature(key = 'leisure', value = 'park') %>%
  osmdata_sf()

parques <- parks$osm_points

##Discotecas

nightclub <- bogota %>%
  add_osm_feature(key = 'amenity', value = 'nightclub') %>%
  osmdata_sf()

discotecas <- nightclub$osm_points

##Colegios

school <- bogota %>%
  add_osm_feature(key = 'amenity', value = 'school') %>%
  osmdata_sf()

colegios <- school$osm_points

##University

university <- bogota %>%
  add_osm_feature(key = 'amenity', value = 'university') %>%
  osmdata_sf()

# Extract university points
universidades <- university$osm_points

## Estaciones de bus

estaciones <- bogota %>%
  add_osm_feature(key = 'amenity', value = 'bus_station') %>%
  osmdata_sf()

bus_station <- estaciones$osm_points


################Join
# Calculate the shortest distance to the nearest restaurant for each property
train$dist_nearest_restaurant <- st_nearest_feature(train, restaurantes)
test$dist_nearest_restaurant <- st_nearest_feature(test, restaurantes)
train$dist_nearest_parques <- st_nearest_feature(train, parques)
test$dist_nearest_parques <- st_nearest_feature(test, parques)
train$dist_nearest_discotecas <- st_nearest_feature(train, discotecas)
test$dist_nearest_discotecas <- st_nearest_feature(test, discotecas)
train$dist_nearest_colegios <- st_nearest_feature(train, colegios)
test$dist_nearest_colegios <- st_nearest_feature(test, colegios)
train$dist_nearest_universidades <- st_nearest_feature(train, universidades)
test$dist_nearest_universidades <- st_nearest_feature(test, universidades)
train$dist_nearest_bus_station <- st_nearest_feature(train, bus_station)
test$dist_nearest_bus_station <- st_nearest_feature(test, bus_station)


# Convert the distances to more readable units (meters or kilometers as required)
train$dist_nearest_restaurant <- st_distance(train, restaurantes[st_nearest_feature(train, restaurantes), ], by_element = TRUE)
test$dist_nearest_restaurant <- st_distance(test, restaurantes[st_nearest_feature(test, restaurantes), ], by_element = TRUE)
train$dist_nearest_parques <- st_distance(train, parques[st_nearest_feature(train, parques), ], by_element = TRUE)
test$dist_nearest_parques <- st_distance(test, parques[st_nearest_feature(test, parques), ], by_element = TRUE)
train$dist_nearest_discotecas <- st_distance(train, discotecas[st_nearest_feature(train, discotecas), ], by_element = TRUE)
test$dist_nearest_discotecas <- st_distance(test, discotecas[st_nearest_feature(test, discotecas), ], by_element = TRUE)
train$dist_nearest_colegios <- st_distance(train, colegios[st_nearest_feature(train, colegios), ], by_element = TRUE)
test$dist_nearest_colegios <- st_distance(test, colegios[st_nearest_feature(test, colegios), ], by_element = TRUE)
train$dist_nearest_universidades <- st_distance(train,universidades[st_nearest_feature(train, universidades), ], by_element = TRUE)
test$dist_nearest_universidades <- st_distance(test,universidades[st_nearest_feature(test, universidades), ], by_element = TRUE)
train$dist_nearest_bus_station <- st_distance(train,bus_station[st_nearest_feature(train, bus_station), ], by_element = TRUE)
test$dist_nearest_bus_station <- st_distance(test,bus_station[st_nearest_feature(test, bus_station), ], by_element = TRUE)


# Define a radius in meters (e.g., 1000 meters)
radius_100 <- 100 #Ajustar

# Create a buffer around each property
train_buffer <- st_buffer(train, dist = radius_100, nQuadSegs = 3)
test_buffer <- st_buffer(test, dist = radius_100, nQuadSegs = 3)

# Count restaurants within the buffer
train$restaurant_100m <- sapply(st_intersects(train_buffer, restaurantes), length)
test$restaurant_100m <- sapply(st_intersects(test_buffer, restaurantes), length)
train$parques_100m <- sapply(st_intersects(train_buffer, parques), length)
test$parques_100m <- sapply(st_intersects(test_buffer, parques), length)
train$discotecas_100m <- sapply(st_intersects(train_buffer, discotecas), length)
test$discotecas_100m <- sapply(st_intersects(test_buffer, discotecas), length)
train$colegios_100m <- sapply(st_intersects(train_buffer, colegios), length)
test$colegios_100m <- sapply(st_intersects(test_buffer, colegios), length)

# Define a radius in meters (e.g., 1000 meters)
radius_300 <- 300 #Ajustar

# Create a buffer around each property
train_buffer <- st_buffer(train, dist = radius_300, nQuadSegs = 3)
test_buffer <- st_buffer(test, dist = radius_300, nQuadSegs = 3)

# Count restaurants within the buffer
train$restaurant_300m <- sapply(st_intersects(train_buffer, restaurantes), length)
test$restaurant_300m <- sapply(st_intersects(test_buffer, restaurantes), length)
train$parques_300m <- sapply(st_intersects(train_buffer, parques), length)
test$parques_300m <- sapply(st_intersects(test_buffer, parques), length)
train$discotecas_300m <- sapply(st_intersects(train_buffer, discotecas), length)
test$discotecas_300m <- sapply(st_intersects(test_buffer, discotecas), length)
train$colegios_300m <- sapply(st_intersects(train_buffer, colegios), length)
test$colegios_300m <- sapply(st_intersects(test_buffer, colegios), length)


localidades <-st_read("data/poligonos-localidades.geojson")
test <- st_join(test, localidades, join = st_within)

# Do the same for the train dataset if needed
train <- st_join(train, localidades, join = st_within)

barrios <-st_read("data/barrios_prueba.geojson")

test <- st_join(test, barrios, join = st_within)

# Do the same for the train dataset if needed
train <- st_join(train, barrios, join = st_within)

estratos <-st_read("data/estrato-socioeconomico-bogota-2019.geojson")

train <- st_join(train, estratos, join = st_within)
test <- st_join(test, estratos, join = st_within)

train_con_estrato <- train[!is.na(train$estrato), ]
train_sin_estrato <- train[is.na(train$estrato), ]

test_con_estrato <- test[!is.na(test$estrato), ]
test_sin_estrato <- test[is.na(test$estrato), ]

# Para el conjunto de entrenamiento
train_coords_con_estrato <- as.data.frame(st_coordinates(train_con_estrato))
train_coords_sin_estrato <- as.data.frame(st_coordinates(train_sin_estrato))

# Para el conjunto de prueba
test_coords_con_estrato <- as.data.frame(st_coordinates(test_con_estrato))
test_coords_sin_estrato <- as.data.frame(st_coordinates(test_sin_estrato))

estrato_predicho_train <- knn(train = train_coords_con_estrato, test = train_coords_sin_estrato, cl = train_con_estrato$estrato, k = 3)
train_sin_estrato$estrato <- estrato_predicho_train

# Imputación para el conjunto de prueba
estrato_predicho_test <- knn(train = test_coords_con_estrato, test = test_coords_sin_estrato, cl = test_con_estrato$estrato, k = 3)
test_sin_estrato$estrato <- estrato_predicho_test

train <- rbind(train_con_estrato, train_sin_estrato)
test <- rbind(test_con_estrato, test_sin_estrato)

# Revisar las tablas de frecuencia de los estratos imputados
table(train$estrato)
table(test$estrato)


st_write(test, "data/test_shp.shp") #Shapefile
st_write(train, "data/train_shp.shp") #Shapefile

st_write(test, "data/test_json_v3.geojson")
st_write(train, "data/train_json_v3.geojson")




tr_city_center <- st_sfc(st_point(c(-74.076100, 4.597859)), crs = st_crs(train$geometry))
te_city_center <- st_sfc(st_point(c(-74.076100, 4.597859)), crs = st_crs(test$geometry))
# Calculate the distance in meters (assuming your CRS uses meters, such as EPSG:3857)
# If your data is in lat/long (EPSG:4326), you might want to transform it to a metric CRS first
train$dis_centro <- st_distance(st_transform(train$geometry, 3857), st_transform(tr_city_center, 3857))
test$dis_centro <- st_distance(st_transform(test$geometry, 3857), st_transform(te_city_center, 3857))

tr_andino <- st_sfc(st_point(c(-74.052829, 4.666689)), crs = st_crs(train$geometry))
te_andino <- st_sfc(st_point(c(-74.052829, 4.666689)), crs = st_crs(test$geometry))
train$dis_andino <- st_distance(st_transform(train$geometry, 3857), st_transform(tr_andino, 3857))
test$dis_andino <- st_distance(st_transform(test$geometry, 3857), st_transform(te_andino, 3857))


radius_1000 <- 1000 #Ajustar

# Create a buffer around each property
train_buffer <- st_buffer(train, dist = radius_1000, nQuadSegs = 3)
test_buffer <- st_buffer(test, dist = radius_1000, nQuadSegs = 3)

# Count restaurants within the buffer
train$restaurant_1km <- sapply(st_intersects(train_buffer, restaurantes), length)
test$restaurant_1km <- sapply(st_intersects(test_buffer, restaurantes), length)
train$parques_1km <- sapply(st_intersects(train_buffer, parques), length)
test$parques_1km <- sapply(st_intersects(test_buffer, parques), length)
train$discotecas_1km <- sapply(st_intersects(train_buffer, discotecas), length)
test$discotecas_1km <- sapply(st_intersects(test_buffer, discotecas), length)
train$colegios_1km <- sapply(st_intersects(train_buffer, colegios), length)
test$colegios_1kmm <- sapply(st_intersects(test_buffer, colegios), length)
train$uni_1km <- sapply(st_intersects(train_buffer, universidades), length)
test$uni_1km <- sapply(st_intersects(test_buffer, universidades), length)
train$bus_station_1km <- sapply(st_intersects(train_buffer, bus_station), length)
test$bus_station_1km <- sapply(st_intersects(test_buffer, bus_station), length)



st_write(test, "data/test_json_v5.geojson")
st_write(train, "data/train_json_v5.geojson")
