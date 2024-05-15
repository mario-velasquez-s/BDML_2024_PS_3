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
user <- Sys.getenv("USERNAME") # or you can hardcode the username if needed
if (user == "Maria.Arias") {
  setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 3/carpeta equipo/BDML_2024_PS_3")
} else if (user == "marti") {
  setwd("C:/Users/marti/OneDrive/Documentos/BDML_2024/BDML_2024/BDML_2024_PS_3_definitivo")
}
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
train_copy <- train
test_copy <- test
str(test)

train_miss <- skim(train)
test_miss <- skim(test)
print(train_miss)
print(test_miss)
# Surface, rooms and bathrooms have significant missings.

###############################################################################
#                           Creation of geographic data    
#                   (Takes to long to run. To avoid running, run
#                line 61 and 64 to get the resulting data frames)
###############################################################################

#source("scripts/geographic_variables.R")


###############################################################################
#                         Start here to save time
###############################################################################
# Read the 'test' shapefile
train <- st_read("data/train_json_barrios.geojson")

# Read the 'train' shapefile
test <- st_read("data/test_json_barrios.geojson")

###############################################################################
#           Imputation and creation of variables from description 
###############################################################################

source("scripts/impu_new_variables.R")

# Explore data ------------------------------------------------------------

round(prop.table(table(train$property_type)),3) ## In the training set 76% are apartments
round(prop.table(table(test$property_type)),3) ## While in the test set 97% are apartments
## Hence in Chapinero people rarely sell houses.

round(prop.table(table(train$rooms_imp_numerico)), 3)
round(prop.table(table(test$rooms_imp_numerico)),3) ## In Chapinero apartments of 1 rooms seem more popular than in the training set.

round(prop.table(table(train$baños)), 3)
round(prop.table(table(test$baños)),3)

## In Chapinero properties seem smaller than in the train set.
summary(train$area)
summary(test$area)

## An instrumental function to estimate mean of variables group_by a particular other variable
groupby_mean <- function(base,grupo_por,variable){
  summary <- base %>%
    group_by({{grupo_por}}) %>%
    summarise(count = n(),
              mean = mean({{variable}}),
              sd = sd({{variable}}),
              min= min({{variable}}),
              max = max({{variable}}))
  print(summary)
}


## We notice there are advertisements for different months and years, in both databases.
## There is no significant variation between years and months
ggplot(data=train,aes(x=factor(month), y=price)) + geom_boxplot() + theme_minimal()
groupby_mean(train,month,price)

## Now, we notice that prices in Chapinero are the highest in the train data 
ggplot(data=train,aes(x=factor(cod_loc), y=price)) + geom_boxplot() + theme_minimal()

#We check if is a matter of area. But it isn't. The mean and the minimum are the highest.
train <- train %>% mutate(price_per_area = price / area)
ggplot(data=train,aes(x=factor(cod_loc), y=price_per_area)) + geom_boxplot() + theme_minimal()
groupby_mean(train,localidad,price_per_area)

# Ideally, we will use only Chapinero properties in the train set, but they're 
# only 307 observations. So, we will...


