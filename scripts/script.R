# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez, Daniela Vlasak
# Problem Set 2
# 

# Initial Setup -----------------------------------------------------------

rm(list = ls())

if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       gridExtra, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, ## tables/output to TEX. 
       MASS,
       rvest,
       httr,
       dplyr,
       ggplot2,
       visdat,
       caret,
       sf,
       osmdata)


# 1: Initial Data Manipulation -----------------------------------------------
user <- Sys.getenv("USERNAME")

if (user == "Maria.Arias") {
  setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 3/data")
} else if (user == "marti") {
  setwd("C:/Users/marti/OneDrive - Universidad de los andes/BDML - Datos/PS3")
}

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train_miss <- skim(train)
test_miss <- skim(test)
print(train_miss)
print(test_miss)
# Surface, rooms and bathrooms have significant missings.

###############################################################################
#                                    Imputation                               
###############################################################################
train %>% count(bathrooms)
ggplot(data=train,aes(x=bedrooms, y=bathrooms)) + geom_point()
ggplot(data=train,aes(x=rooms, y=bathrooms)) + geom_point()
filtered_data <- subset(train, surface_total < 200)
ggplot(data=filtered_data,aes(x=surface_total, y=bathrooms)) + geom_point()

##There is no lineal sense with the bathrooms variable, so, I will 


#Explore data
prop.table(table(train$property_type)) ## In the training set 76% are apartments
prop.table(table(test$property_type)) ## While in the test set 97% are apartments
## Hence in Chapinero people rarely sell houses.

###############################################################################
#                                    Datos espaciales                             
###############################################################################

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

#Comentario prueba
