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



# Convert the distances to more readable units (meters or kilometers as required)
train$dist_nearest_restaurant <- st_distance(train, restaurantes[st_nearest_feature(train, restaurantes), ], by_element = TRUE)
test$dist_nearest_restaurant <- st_distance(test, restaurantes[st_nearest_feature(test, restaurantes), ], by_element = TRUE)
train$dist_nearest_parques <- st_distance(train, parques[st_nearest_feature(train, parques), ], by_element = TRUE)
test$dist_nearest_parques <- st_distance(test, parques[st_nearest_feature(test, parques), ], by_element = TRUE)
train$dist_nearest_discotecas <- st_distance(train, discotecas[st_nearest_feature(train, discotecas), ], by_element = TRUE)
test$dist_nearest_discotecas <- st_distance(test, discotecas[st_nearest_feature(test, discotecas), ], by_element = TRUE)
train$dist_nearest_colegios <- st_distance(train, colegios[st_nearest_feature(train, colegios), ], by_element = TRUE)
test$dist_nearest_colegios <- st_distance(test, colegios[st_nearest_feature(test, colegios), ], by_element = TRUE)



# Define a radius in meters (e.g., 1000 meters)
radius <- 1000 #Ajustar

# Create a buffer around each property
train_buffer <- st_buffer(train, dist = radius)
test_buffer <- st_buffer(test, dist = radius)

# Count restaurants within the buffer
train$restaurant_density <- sapply(st_intersects(train_buffer, restaurantes), length)
test$restaurant_density <- sapply(st_intersects(test_buffer, restaurantes), length)
train$parques_density <- sapply(st_intersects(train_buffer, parques), length)
test$parques_density <- sapply(st_intersects(test_buffer, parques), length)
train$discotecas_density <- sapply(st_intersects(train_buffer, discotecas), length)
test$discotecas_density <- sapply(st_intersects(test_buffer, discotecas), length)
train$colegios_density <- sapply(st_intersects(train_buffer, colegios), length)
test$colegios_density <- sapply(st_intersects(test_buffer, colegios), length)


