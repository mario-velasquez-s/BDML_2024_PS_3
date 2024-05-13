# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez
# Problem Set 3
# 
# Creation of geographic data using OPEN MAPS

test_espacial <- test %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

train_espacial <- train %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

bogota<-opq(bbox = getbb("Bogotá Colombia"))

##Restaurantes

restaurants <- bogota %>%
  add_osm_feature(key = 'amenity', value = 'restaurant') %>%
  osmdata_sf()

restaurantes <- restaurants$osm_points
ggplot()+
  geom_sf(data=restaurantes) +
  theme_bw()

##Parques

parks <- bogota %>%
  add_osm_feature(key = 'leisure', value = 'park') %>%
  osmdata_sf()

parques <- parks$osm_points
ggplot()+
  geom_sf(data=parques) +
  theme_bw()

##Discotecas

nightclub <- bogota %>%
  add_osm_feature(key = 'amenity', value = 'nightclub') %>%
  osmdata_sf()

discotecas <- nightclub$osm_points
ggplot()+
  geom_sf(data=discotecas) +
  theme_bw()

##Colegios

school <- bogota %>%
  add_osm_feature(key = 'amenity', value = 'school') %>%
  osmdata_sf()

colegios <- school$osm_points
ggplot()+
  geom_sf(data=school) +
  theme_bw()

################Join

# Calculate the shortest distance to the nearest restaurant for each property
train_espacial$dist_nearest_restaurant <- st_nearest_feature(train_espacial, restaurantes)
test_espacial$dist_nearest_restaurant <- st_nearest_feature(test_espacial, restaurantes)
train_espacial$dist_nearest_parques <- st_nearest_feature(train_espacial, parques)
test_espacial$dist_nearest_parques <- st_nearest_feature(test_espacial, parques)
train_espacial$dist_nearest_discotecas <- st_nearest_feature(train_espacial, discotecas)
test_espacial$dist_nearest_discotecas <- st_nearest_feature(test_espacial, discotecas)
train_espacial$dist_nearest_colegios <- st_nearest_feature(train_espacial, colegios)
test_espacial$dist_nearest_colegios <- st_nearest_feature(test_espacial, colegios)



# Convert the distances to more readable units (meters or kilometers as required)
train_espacial$dist_nearest_restaurant <- st_distance(train_espacial, restaurantes[st_nearest_feature(train_espacial, restaurantes), ], by_element = TRUE)
test_espacial$dist_nearest_restaurant <- st_distance(test_espacial, restaurantes[st_nearest_feature(test_espacial, restaurantes), ], by_element = TRUE)
train_espacial$dist_nearest_parques <- st_distance(train_espacial, parques[st_nearest_feature(train_espacial, parques), ], by_element = TRUE)
test_espacial$dist_nearest_parques <- st_distance(test_espacial, parques[st_nearest_feature(test_espacial, parques), ], by_element = TRUE)
train_espacial$dist_nearest_discotecas <- st_distance(train_espacial, discotecas[st_nearest_feature(train_espacial, discotecas), ], by_element = TRUE)
test_espacial$dist_nearest_discotecas <- st_distance(test_espacial, discotecas[st_nearest_feature(test_espacial, discotecas), ], by_element = TRUE)
train_espacial$dist_nearest_colegios <- st_distance(train_espacial, colegios[st_nearest_feature(train_espacial, colegios), ], by_element = TRUE)
test_espacial$dist_nearest_colegios <- st_distance(test_espacial, colegios[st_nearest_feature(test_espacial, colegios), ], by_element = TRUE)



# Define a radius in meters (e.g., 1000 meters)
radius <- 1000 #Ajustar

# Create a buffer around each property
train_buffer <- st_buffer(train_espacial, dist = radius)
test_buffer <- st_buffer(test_espacial, dist = radius)

# Count restaurants within the buffer
train_espacial$restaurant_density <- sapply(st_intersects(train_buffer, restaurantes), length)
test_espacial$restaurant_density <- sapply(st_intersects(test_buffer, restaurantes), length)
train_espacial$parques_density <- sapply(st_intersects(train_buffer, parques), length)
test_espacial$parques_density <- sapply(st_intersects(test_buffer, parques), length)
train_espacial$discotecas_density <- sapply(st_intersects(train_buffer, discotecas), length)
test_espacial$discotecas_density <- sapply(st_intersects(test_buffer, discotecas), length)
train_espacial$colegios_density <- sapply(st_intersects(train_buffer, colegios), length)
test_espacial$colegios_density <- sapply(st_intersects(test_buffer, colegios), length)


