# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez
# Problem Set 3
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
if (user == "Maria.Arias") {
  setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 3/data")
} else if (user == "marti") {
  setwd("C:/Users/marti/OneDrive - Universidad de los andes/BDML - Datos/PS3")
}
train <- read.csv("train.csv")
test <- read.csv("test.csv")
str(test)

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

##There is no lineal sense with the bathrooms variable, so, I will recover variables from property description

## Text to lowercase
limpieza_texto <- function(base){
  base <- base %>% mutate(description = str_to_lower(description) )
  base <- base %>% mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
  return(base)
}

train <- limpieza_texto(train)
test <- limpieza_texto(test)

## Creating the number of floors variable:
numeros_escritos <- c( "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
numeros_numericos <- as.character(2:10)

var_npisos <-function(base){
  base <- base %>% mutate(n_pisos = str_extract(description,"(\\w+|\\d+) pisos")) %>%
    mutate(n_pisos = ifelse(property_type == "Casa", n_pisos,NA))
  
  #Converting text numbers to digits
  base <- base %>% mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))
  
  #We only keep numbers and put 1 floor for NAs
  base <- base %>% mutate(n_pisos_numerico = as.integer(str_extract(n_pisos,"\\d+"))) %>%
                    mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
                    mutate(n_pisos_numerico = if_else(n_pisos_numerico>10, 1, n_pisos_numerico))
  
  return(base)
}

train <- var_npisos(train)
test <- var_npisos(test)

ggplot(train, aes(x = factor(n_pisos_numerico))) +
  geom_bar() +
  labs(title="", x="Pisos", y="Obs") + 
  theme_minimal()

prop.table(table(train$n_pisos_numerico)) 
prop.table(table(test$n_pisos_numerico)) ## Both in training and test the properties seem to be of only one floor.



# Imputing information about number of rooms
rooms_bedrooms <- train %>% mutate(diff = rooms - bedrooms, na.rm=TRUE) %>% 
  dplyr::select(diff)
summary(rooms_bedrooms$diff) ## En realidad puedo reemplazar la variable rooms con bedrooms, la diferencia es de 0.005 en promedio.

numeros_escritos <- c( "una","un","dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
dos_diez <- as.character(2:10)
numeros_numericos <- c(1,1,dos_diez)

var_rooms_imp <-function(base){
  base <- base %>% mutate(rooms_imp = str_extract(description, regex("(\\w+|\\d+) (habitaciones|alcobas|cuartos|habitacion|alcoba|cuarto)")))
  base <- base %>% mutate(rooms_imp = if_else(is.na(rooms_imp), str_extract(title, regex("(\\w+|\\d+) (habitaciones|alcobas|cuartos|habitacion|alcoba|cuarto)")), rooms_imp))
  #Until this point we recovered 80.3% of rooms data. 
  
  #Converting text numbers to digits
  base <- base %>% mutate(rooms_imp = str_replace_all(rooms_imp, setNames(numeros_numericos,numeros_escritos)))
  
  #We only keep numbers and impute NAs with variables bedrooms
  base <- base %>% mutate(rooms_imp_numerico = as.integer(str_extract(rooms_imp,"\\d+"))) %>%
    mutate(rooms_imp_numerico = if_else(is.na(rooms_imp_numerico), rooms, rooms_imp_numerico)) %>%
    mutate(rooms_imp_numerico = if_else(is.na(rooms_imp_numerico), bedrooms, rooms_imp_numerico)) %>%
    mutate(rooms_imp_numerico = if_else(rooms_imp_numerico>10, 1, rooms_imp_numerico))
  
  
  return(base)
}

train <- var_rooms_imp(train)
test <- var_rooms_imp(test)

ggplot(test, aes(x = factor(rooms_imp_numerico))) +
  geom_bar() +
  labs(title="", x="Cuartos", y="Obs") + 
  theme_minimal() ## In the test set most common are 1-3 rooms, while in train 2-4 rooms.

prop.table(table(train$rooms_imp_numerico)) 
prop.table(table(test$rooms_imp_numerico)) ## In Chapinero apartments of 1 rooms seem more popular than in the training set.
summary(train$rooms_imp_numerico)
summary(test$rooms_imp_numerico)



# Imputing information about area
total_covered <- train %>% mutate(diff = surface_total - surface_covered, na.rm=TRUE) %>% 
  dplyr::select(diff)
summary(total_covered$diff) ## Total surface and covered are almost the same. Their mean difference is 4.75 m2.
                            ## I will replace first by the description, and then by the covered.


var_surface <-function(base){
  base <- base %>% mutate(surface = str_extract(description, "\\d+(?=\\s*(mts2|mts|mt2|mt|m2))"))
  base <- base %>% mutate(surface = if_else(is.na(surface), str_extract(title, "\\d+(?=\\s*(mts2|mts|mt2|mt|m2))"), surface))
  #Until this point we recovered 13% of surface data. 
  
  #We only keep numbers and impute NAs with variables bedrooms
  base <- base %>% mutate(area = surface_covered) %>%
    mutate(area = if_else(is.na(area), as.integer(str_extract(surface,"\\d+")), area)) %>%
    mutate(area = if_else(is.na(area), surface_total, area)) %>%
    mutate(area = if_else(area>400, NA, area))
  
  
  return(base)
}

train <- var_surface(train)
test <- var_surface(test)


# Crear histograma con datos combinados
combined_data <- rbind(
  transform(train, dataset = "train"),
  transform(test, dataset = "test")
  
)

histogram_comparativo <- ggplot(combined_data, aes(x = area, fill = dataset)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "", x = "Área (mt2)", y = "Obs", fill = "Dataset") +
  theme_minimal()
histogram_comparativo
## In Chapinero properties seem smaller than in the train set.
summary(train$area)
summary(test$area)

#Still lack 49.6% of area/surface information

ggplot(data=test,aes(x=factor(rooms_imp_numerico), y=area)) + geom_boxplot() # We observe rooms could help us to recover this information

area_with_rooms <- function(base){
  base <- base %>% 
    group_by(rooms_imp_numerico) %>% 
    mutate(mean_area = mean(area, na.rm=T)) %>%
    ungroup()
  base <- base %>%
    mutate(area = ifelse(is.na(area) == T, yes = mean_area, no = area))
  return(base)
}

train <- area_with_rooms(train)
test <- area_with_rooms(test)


## I only keep variables of my interest
useful_vars <- function(base){
  base <- base %>% dplyr::select(-surface_total,-surface_covered, -rooms, -mean_area,
                                 -n_pisos, -rooms_imp,-surface)
  return(base)
}
train <- useful_vars(train)
test <- useful_vars(test)

# ------------------------------------------------------------------------------


# Geographic Data ---------------------------------------------------------


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



# Explore data ------------------------------------------------------------


#Explore data
prop.table(table(train$property_type)) ## In the training set 76% are apartments
prop.table(table(test$property_type)) ## While in the test set 97% are apartments
## Hence in Chapinero people rarely sell houses.


