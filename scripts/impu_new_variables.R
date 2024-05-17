# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez
# Problem Set 3
# 
# Creation of new variables based on text and imputation
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

ggplot(train, aes(x = factor(rooms_imp_numerico))) +
  geom_bar() +
  labs(title="", x="Cuartos", y="Obs") + 
  theme_minimal() ## In the test set most common are 1-3 rooms, while in train 2-4 rooms.

prop.table(table(train$rooms_imp_numerico)) 
prop.table(table(test$rooms_imp_numerico)) ## In Chapinero apartments of 1 rooms seem more popular than in the training set.
summary(train$rooms_imp_numerico)
summary(test$rooms_imp_numerico)



# Imputing information about number of bathrooms

numeros_escritos <- c( "y","su","con","un","dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
dos_diez <- as.character(2:10)
numeros_numericos <- c(1,1,1,1,dos_diez)

var_baños <-function(base){
  base <- base %>% mutate(baños_imp =str_extract(description, regex("(\\w+|\\d+) (bano|banos)")))
  base <- base %>% mutate(baños_imp = if_else(is.na(baños_imp), str_extract(title, regex("(\\w+|\\d+) (bano|banos)")), baños_imp))
  
  #Converting text numbers to digits
  base <- base %>% mutate(baños_imp = str_replace_all(baños_imp, setNames(numeros_numericos,numeros_escritos)))
  
  #We only keep numbers and impute NAs with variables bedrooms
  base <- base %>% mutate(baño_imp_numerico = as.integer(str_extract(baños_imp,"\\d+")))
  #Until this point we recovered 38,9% of rooms data. I now will impute with the variable "bathrooms"
  
  base <- base %>% mutate(baños = if_else(is.na(bathrooms),baño_imp_numerico,bathrooms)) %>%
    dplyr::select(-baño_imp_numerico,-baños_imp)%>%
    mutate(baños = if_else(baños>10, 1, baños))
  #we recovered 86,6% of rooms data. I now will impute with the variable "bathrooms"
  
  base <- base %>% mutate(baños = if_else(is.na(baños),1,baños))
  
  return(base)
}

train <- var_baños(train)
test <- var_baños(test)

ggplot(test, aes(x = factor(baños))) +
  geom_bar() +
  labs(title="", x="Pisos", y="Obs") + 
  theme_minimal()


# Creating a variable of squared number of rooms 
var_sq_rooms <- function(base){
  
  base <- base %>% mutate(sq_rooms = rooms_imp_numerico^2,
                          sq_baños = baños^2)
  return(base)
  
}

train <- var_sq_rooms(train)
test <- var_sq_rooms(test)



# Imputing information about area
total_covered <- train %>% mutate(diff = surface_total - surface_covered, na.rm=TRUE) %>% 
  dplyr::select(diff)
summary(total_covered$diff) ## Total surface and covered are almost the same. Their mean difference is 4.75 m2.
## I will replace first based in the description, and then by the covered.

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

ggplot(data=test,aes(x=factor(bathrooms), y=area)) + geom_boxplot() # We observe bathrooms could help us to recover this information

area_with_rooms <- function(base){
  base <- base %>% 
    group_by(bathrooms) %>% 
    mutate(mean_area = mean(area, na.rm=T)) %>%
    ungroup()
  base <- base %>%
    mutate(area = ifelse(is.na(area) == T, yes = mean_area, no = area))
  return(base)
}

train <- area_with_rooms(train)
test <- area_with_rooms(test)

train <- train %>% filter(!is.na(area))


# Imputing information about number of bathrooms

var_terraza <-function(base){
  base <- base %>% mutate(terraza_signal =str_extract(description, regex("(terraza|terrazas|balcon)")))
  base <- base %>% mutate(terraza = if_else(is.na(terraza_signal), 0, 1)) %>% dplyr::select(-terraza_signal)
  
  return(base)
}

train <- var_terraza(train)
test <- var_terraza(test)

#Create a variable if it has significant ilumination (that's important in Bogotá)
var_iluminado <-function(base){
  base <- base %>% mutate(ilu_signal =str_extract(description, regex("(iluminado|iluminados|iluminadas)")))
  base <- base %>% mutate(iluminado = if_else(is.na(ilu_signal), 0, 1)) %>% dplyr::select(-ilu_signal)
  
  return(base)
}

train <- var_iluminado(train)
test <- var_iluminado(test)


#Create a variable if it was recently refurbished
var_remo <-function(base){
  base <- base %>% mutate(remo_signal =str_extract(description, regex("(remodelado|renovado|cambiado|cambiados|remodeladas|remodelada|renovados|nuevo|nuevos|nuevas|nueva)")))
  base <- base %>% mutate(remodelado = if_else(is.na(remo_signal), 0, 1)) %>% dplyr::select(-remo_signal)
  
  return(base)
}

train <- var_remo(train)
test <- var_remo(test)

#Create a variable if it has lift
var_lift <-function(base){
  base <- base %>% mutate(lift_signal =str_extract(description, regex("(ascensor|asensor)")))
  base <- base %>% mutate(ascensor = if_else(is.na(lift_signal), 0, 1)) %>% dplyr::select(-lift_signal)
  
  return(base)
}

train <- var_lift(train)
test <- var_lift(test)



## I only keep variables of my interest
train_miss <- skim(train)
test_miss <- skim(test)
print(train_miss)
print(test_miss)

#Localidad description is missing, I impute them with cod_loc and erase the 255 that I don't have information.
sin_localidad <- train %>% filter(is.na(localidad)) %>% dplyr::select(cod_loc, localidad)
unique(sin_localidad$cod_loc)
table(sin_localidad$cod_loc) #Falta Suba

table(train$localidad,train$cod_loc)

imp_localidad <- function(base){
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==1, "Usaquén",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==2, "Chapinero",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==3, "Candelaria",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==4, "San Cristóbal",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==6, "Tunjuelito",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==7, "Bosa",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==8, "Kennedy",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==9, "Fontibón",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==10, "Engativá",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==11, "Suba",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==12, "Barrios Unidos",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==13, "Teusaquillo",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==14, "Los Mártires",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==15, "Antonio Nariño",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==16, "Puente Aranda",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==18, "Rafael Uribe",localidad))
  base <- base %>% mutate(localidad = ifelse(is.na(localidad) & cod_loc==19, "Ciudad Bolívar",localidad))
  
  return(base)
}

train <- imp_localidad(train)
test <- imp_localidad(test)


#Variables as factors
imp_factors <- function(base){
  base <- base %>% mutate(property_type = factor(base$property_type, levels= c("Apartamento","Casa")))
  base <- base %>% mutate(localidad = factor(base$localidad))
}
train <- imp_factors(train)
test <- imp_factors(test)

train

useful_vars <- function(base){
  base <- base %>% dplyr::select(-surface_total,-surface_covered, -rooms, -mean_area,
                                 -n_pisos, -rooms_imp,-surface, -bathrooms,-bedrooms)
  return(base)
}
train <- useful_vars(train)
test <- useful_vars(test)

# ------------------------------------------------------------------------------
